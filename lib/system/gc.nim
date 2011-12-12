#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#


#            Garbage Collector
#
# The basic algorithm is *Deferrent Reference Counting* with cycle detection.
# This is achieved by combining a Deutsch-Bobrow garbage collector
# together with Christoper's partial mark-sweep garbage collector.
#
# Special care has been taken to avoid recursion as far as possible to avoid
# stack overflows when traversing deep datastructures. This is comparable to
# an incremental and generational GC. It should be well-suited for soft real
# time applications (like games).

const
  CycleIncrease = 2 # is a multiplicative increase
  InitialCycleThreshold = 4*1024*1024 # X MB because cycle checking is slow
  ZctThreshold = 500  # we collect garbage if the ZCT's size
                      # reaches this threshold
                      # this seems to be a good value

const
  rcIncrement = 0b1000 # so that lowest 3 bits are not touched
  # NOTE: Most colors are currently unused
  rcBlack = 0b000  # cell is colored black; in use or free
  rcGray = 0b001   # possible member of a cycle
  rcWhite = 0b010  # member of a garbage cycle
  rcPurple = 0b011 # possible root of a cycle
  rcZct = 0b100    # in ZCT
  rcMarked = 0b101 # dummy write to keep C code generator from
                   # eliminating the root
  rcRed = 0b101    # Candidate cycle undergoing sigma-computation
  rcOrange = 0b110 # Candidate cycle awaiting epoch boundary
  rcShift = 3      # shift by rcShift to get the reference counter
  colorMask = 0b111
type
  TWalkOp = enum
    waZctDecRef, waPush, waCycleDecRef

  TFinalizer {.compilerproc.} = proc (self: pointer)
    # A ref type can have a finalizer that is called before the object's
    # storage is freed.

  TGcStat {.final, pure.} = object
    stackScans: int          # number of performed stack scans (for statistics)
    cycleCollections: int    # number of performed full collections
    maxThreshold: int        # max threshold that has been set
    maxStackSize: int        # max stack size
    maxStackCells: int       # max stack cells in ``decStack``
    cycleTableSize: int      # max entries in cycle table  
  
  TGcHeap {.final, pure.} = object # this contains the zero count and
                                   # non-zero count table
    stackBottom: pointer
    cycleThreshold: int
    zct: TCellSeq            # the zero count table
    decStack: TCellSeq       # cells in the stack that are to decref again
    cycleRoots: TCellSet
    tempStack: TCellSeq      # temporary stack for recursion elimination
    recGcLock: int           # prevent recursion via finalizers; no thread lock
    region: TMemRegion       # garbage collected region
    stat: TGcStat

var
  gch {.rtlThreadVar.}: TGcHeap

when not defined(useNimRtl):
  InstantiateForRegion(gch.region)

proc acquire(gch: var TGcHeap) {.inline.} = 
  when hasThreadSupport and hasSharedHeap:
    AcquireSys(HeapLock)

proc release(gch: var TGcHeap) {.inline.} = 
  when hasThreadSupport and hasSharedHeap:
    releaseSys(HeapLock)

proc addZCT(s: var TCellSeq, c: PCell) {.noinline.} =
  if (c.refcount and rcZct) == 0:
    c.refcount = c.refcount and not colorMask or rcZct
    add(s, c)

proc cellToUsr(cell: PCell): pointer {.inline.} =
  # convert object (=pointer to refcount) to pointer to userdata
  result = cast[pointer](cast[TAddress](cell)+%TAddress(sizeof(TCell)))

proc usrToCell(usr: pointer): PCell {.inline.} =
  # convert pointer to userdata to object (=pointer to refcount)
  result = cast[PCell](cast[TAddress](usr)-%TAddress(sizeof(TCell)))

proc canbeCycleRoot(c: PCell): bool {.inline.} =
  result = ntfAcyclic notin c.typ.flags

proc extGetCellType(c: pointer): PNimType {.compilerproc.} =
  # used for code generation concerning debugging
  result = usrToCell(c).typ

proc internRefcount(p: pointer): int {.exportc: "getRefcount".} =
  result = int(usrToCell(p).refcount) shr rcShift

# this that has to equals zero, otherwise we have to round up UnitsPerPage:
when BitsPerPage mod (sizeof(int)*8) != 0:
  {.error: "(BitsPerPage mod BitsPerUnit) should be zero!".}

when debugGC:
  proc writeCell(msg: CString, c: PCell) =
    var kind = -1
    if c.typ != nil: kind = ord(c.typ.kind)
    when debugGC:
      c_fprintf(c_stdout, "[GC] %s: %p %d rc=%ld from %s(%ld)\n",
                msg, c, kind, c.refcount shr rcShift, c.filename, c.line)
    else:
      c_fprintf(c_stdout, "[GC] %s: %p %d rc=%ld\n",
                msg, c, kind, c.refcount shr rcShift)

when traceGC:
  # traceGC is a special switch to enable extensive debugging
  type
    TCellState = enum
      csAllocated, csZctFreed, csCycFreed
  var
    states: array[TCellState, TCellSet]

  proc traceCell(c: PCell, state: TCellState) =
    case state
    of csAllocated:
      if c in states[csAllocated]:
        writeCell("attempt to alloc an already allocated cell", c)
        sysAssert(false, "traceCell 1")
      excl(states[csCycFreed], c)
      excl(states[csZctFreed], c)
    of csZctFreed:
      if c in states[csZctFreed]:
        writeCell("attempt to free zct cell twice", c)
        sysAssert(false, "traceCell 2")
      if c in states[csCycFreed]:
        writeCell("attempt to free with zct, but already freed with cyc", c)
        sysAssert(false, "traceCell 3")
      if c notin states[csAllocated]:
        writeCell("attempt to free not an allocated cell", c)
        sysAssert(false, "traceCell 4")
      excl(states[csAllocated], c)
    of csCycFreed:
      if c notin states[csAllocated]:
        writeCell("attempt to free a not allocated cell", c)
        sysAssert(false, "traceCell 5")
      if c in states[csCycFreed]:
        writeCell("attempt to free cyc cell twice", c)
        sysAssert(false, "traceCell 6")
      if c in states[csZctFreed]:
        writeCell("attempt to free with cyc, but already freed with zct", c)
        sysAssert(false, "traceCell 7")
      excl(states[csAllocated], c)
    incl(states[state], c)

  proc writeLeakage() =
    var z = 0
    var y = 0
    var e = 0
    for c in elements(states[csAllocated]):
      inc(e)
      if c in states[csZctFreed]: inc(z)
      elif c in states[csCycFreed]: inc(y)
      else: writeCell("leak", c)
    cfprintf(cstdout, "Allocations: %ld; ZCT freed: %ld; CYC freed: %ld\n",
             e, z, y)

template gcTrace(cell, state: expr): stmt =
  when traceGC: traceCell(cell, state)

# -----------------------------------------------------------------------------

# forward declarations:
proc collectCT(gch: var TGcHeap)
proc IsOnStack*(p: pointer): bool {.noinline.}
proc forAllChildren(cell: PCell, op: TWalkOp)
proc doOperation(p: pointer, op: TWalkOp)
proc forAllChildrenAux(dest: Pointer, mt: PNimType, op: TWalkOp)
# we need the prototype here for debugging purposes

when hasThreadSupport and hasSharedHeap:
  template `--`(x: expr): expr = atomicDec(x, rcIncrement) <% rcIncrement
  template `++`(x: expr): stmt = discard atomicInc(x, rcIncrement)
else:
  template `--`(x: expr): expr = 
    Dec(x, rcIncrement)
    x <% rcIncrement
  template `++`(x: expr): stmt = Inc(x, rcIncrement)

proc prepareDealloc(cell: PCell) =
  if cell.typ.finalizer != nil:
    # the finalizer could invoke something that
    # allocates memory; this could trigger a garbage
    # collection. Since we are already collecting we
    # prevend recursive entering here by a lock.
    # XXX: we should set the cell's children to nil!
    inc(gch.recGcLock)
    (cast[TFinalizer](cell.typ.finalizer))(cellToUsr(cell))
    dec(gch.recGcLock)

proc rtlAddCycleRoot(c: PCell) {.rtl, inl.} = 
  # we MUST access gch as a global here, because this crosses DLL boundaries!
  when hasThreadSupport and hasSharedHeap:
    AcquireSys(HeapLock)
  incl(gch.cycleRoots, c)
  when hasThreadSupport and hasSharedHeap:
    ReleaseSys(HeapLock)

proc rtlAddZCT(c: PCell) {.rtl, inl.} =
  # we MUST access gch as a global here, because this crosses DLL boundaries!
  when hasThreadSupport and hasSharedHeap:
    AcquireSys(HeapLock)
  addZCT(gch.zct, c)
  when hasThreadSupport and hasSharedHeap:
    ReleaseSys(HeapLock)

proc decRef(c: PCell) {.inline.} =
  when stressGC:
    if c.refcount <% rcIncrement:
      writeCell("broken cell", c)
  sysAssert(c.refcount >=% rcIncrement, "decRef")
  #if c.refcount <% rcIncrement: quit("leck mich")
  if --c.refcount:
    rtlAddZCT(c)
  elif canBeCycleRoot(c):
    rtlAddCycleRoot(c) 

proc incRef(c: PCell) {.inline.} = 
  ++c.refcount
  if canBeCycleRoot(c):
    rtlAddCycleRoot(c)

proc nimGCref(p: pointer) {.compilerProc, inline.} = incRef(usrToCell(p))
proc nimGCunref(p: pointer) {.compilerProc, inline.} = decRef(usrToCell(p))

proc asgnRef(dest: ppointer, src: pointer) {.compilerProc, inline.} =
  # the code generator calls this proc!
  sysAssert(not isOnStack(dest), "asgnRef")
  # BUGFIX: first incRef then decRef!
  if src != nil: incRef(usrToCell(src))
  if dest[] != nil: decRef(usrToCell(dest[]))
  dest[] = src

proc asgnRefNoCycle(dest: ppointer, src: pointer) {.compilerProc, inline.} =
  # the code generator calls this proc if it is known at compile time that no 
  # cycle is possible.
  if src != nil: 
    var c = usrToCell(src)
    ++c.refcount
  if dest[] != nil: 
    var c = usrToCell(dest[])
    if --c.refcount:
      rtlAddZCT(c)
  dest[] = src

proc unsureAsgnRef(dest: ppointer, src: pointer) {.compilerProc.} =
  # unsureAsgnRef updates the reference counters only if dest is not on the
  # stack. It is used by the code generator if it cannot decide wether a
  # reference is in the stack or not (this can happen for var parameters).
  if not IsOnStack(dest):
    if src != nil: incRef(usrToCell(src))
    # XXX finally use assembler for the stack checking instead!
    # the test for '!= nil' is correct, but I got tired of the segfaults
    # resulting from the crappy stack checking:
    if cast[int](dest[]) >=% PageSize: decRef(usrToCell(dest[]))
  dest[] = src

proc initGC() =
  when not defined(useNimRtl):
    when traceGC:
      for i in low(TCellState)..high(TCellState): Init(states[i])
    gch.cycleThreshold = InitialCycleThreshold
    gch.stat.stackScans = 0
    gch.stat.cycleCollections = 0
    gch.stat.maxThreshold = 0
    gch.stat.maxStackSize = 0
    gch.stat.maxStackCells = 0
    gch.stat.cycleTableSize = 0
    # init the rt
    init(gch.zct)
    init(gch.tempStack)
    Init(gch.cycleRoots)
    Init(gch.decStack)

proc forAllSlotsAux(dest: pointer, n: ptr TNimNode, op: TWalkOp) =
  var d = cast[TAddress](dest)
  case n.kind
  of nkSlot: forAllChildrenAux(cast[pointer](d +% n.offset), n.typ, op)
  of nkList:
    for i in 0..n.len-1: forAllSlotsAux(dest, n.sons[i], op)
  of nkCase:
    var m = selectBranch(dest, n)
    if m != nil: forAllSlotsAux(dest, m, op)
  of nkNone: sysAssert(false, "forAllSlotsAux")

proc forAllChildrenAux(dest: Pointer, mt: PNimType, op: TWalkOp) =
  var d = cast[TAddress](dest)
  if dest == nil: return # nothing to do
  if ntfNoRefs notin mt.flags:
    case mt.Kind
    of tyRef, tyString, tySequence: # leaf:
      doOperation(cast[ppointer](d)[], op)
    of tyObject, tyTuple:
      forAllSlotsAux(dest, mt.node, op)
    of tyArray, tyArrayConstr, tyOpenArray:
      for i in 0..(mt.size div mt.base.size)-1:
        forAllChildrenAux(cast[pointer](d +% i *% mt.base.size), mt.base, op)
    else: nil

proc forAllChildren(cell: PCell, op: TWalkOp) =
  sysAssert(cell != nil, "forAllChildren: 1")
  sysAssert(cell.typ != nil, "forAllChildren: 2")
  sysAssert cell.typ.kind in {tyRef, tySequence, tyString}, "forAllChildren: 3"
  case cell.typ.Kind
  of tyRef: # common case
    forAllChildrenAux(cellToUsr(cell), cell.typ.base, op)
  of tySequence:
    var d = cast[TAddress](cellToUsr(cell))
    var s = cast[PGenericSeq](d)
    if s != nil:
      for i in 0..s.len-1:
        forAllChildrenAux(cast[pointer](d +% i *% cell.typ.base.size +%
          GenericSeqSize), cell.typ.base, op)
  else: nil

proc addNewObjToZCT(res: PCell, gch: var TGcHeap) {.inline.} =
  # we check the last 8 entries (cache line) for a slot that could be reused.
  # In 63% of all cases we succeed here! But we have to optimize the heck
  # out of this small linear search so that ``newObj`` is not slowed down.
  # 
  # Slots to try          cache hit
  # 1                     32%
  # 4                     59%
  # 8                     63%
  # 16                    66%
  # all slots             68%
  var L = gch.zct.len
  var d = gch.zct.d
  when true:
    # loop unrolled for performance:
    template replaceZctEntry(i: expr) =
      c = d[i]
      if c.refcount >=% rcIncrement:
        c.refcount = c.refcount and not colorMask
        d[i] = res
        return
    if L > 8:
      var c: PCell
      replaceZctEntry(L-1)
      replaceZctEntry(L-2)
      replaceZctEntry(L-3)
      replaceZctEntry(L-4)
      replaceZctEntry(L-5)
      replaceZctEntry(L-6)
      replaceZctEntry(L-7)
      replaceZctEntry(L-8)
      add(gch.zct, res)
    else:
      d[L] = res
      inc(gch.zct.len)
  else:
    for i in countdown(L-1, max(0, L-8)):
      var c = d[i]
      if c.refcount >=% rcIncrement:
        c.refcount = c.refcount and not colorMask
        d[i] = res
        return
    add(gch.zct, res)

proc rawNewObj(typ: PNimType, size: int, gch: var TGcHeap): pointer =
  # generates a new object and sets its reference counter to 0
  acquire(gch)
  sysAssert(typ.kind in {tyRef, tyString, tySequence}, "newObj: 1")
  collectCT(gch)
  var res = cast[PCell](rawAlloc(gch.region, size + sizeof(TCell)))
  sysAssert((cast[TAddress](res) and (MemAlign-1)) == 0, "newObj: 2")
  # now it is buffered in the ZCT
  res.typ = typ
  when debugGC and not hasThreadSupport:
    if framePtr != nil and framePtr.prev != nil:
      res.filename = framePtr.prev.filename
      res.line = framePtr.prev.line
  res.refcount = rcZct # refcount is zero, but mark it to be in the ZCT  
  sysAssert(isAllocatedPtr(gch.region, res), "newObj: 3")
  # its refcount is zero, so add it to the ZCT:
  addNewObjToZCT(res, gch)
  when logGC: writeCell("new cell", res)
  gcTrace(res, csAllocated)
  release(gch)
  result = cellToUsr(res)

proc newObj(typ: PNimType, size: int): pointer {.compilerRtl.} =
  result = rawNewObj(typ, size, gch)
  zeroMem(result, size)

proc newSeq(typ: PNimType, len: int): pointer {.compilerRtl.} =
  # `newObj` already uses locks, so no need for them here.
  result = newObj(typ, addInt(mulInt(len, typ.base.size), GenericSeqSize))
  cast[PGenericSeq](result).len = len
  cast[PGenericSeq](result).space = len

proc newObjRC1(typ: PNimType, size: int): pointer {.compilerRtl.} =
  # generates a new object and sets its reference counter to 1
  acquire(gch)
  sysAssert(typ.kind in {tyRef, tyString, tySequence}, "newObj: 1")
  collectCT(gch)
  var res = cast[PCell](rawAlloc(gch.region, size + sizeof(TCell)))
  sysAssert((cast[TAddress](res) and (MemAlign-1)) == 0, "newObj: 2")
  # now it is buffered in the ZCT
  res.typ = typ
  when debugGC and not hasThreadSupport:
    if framePtr != nil and framePtr.prev != nil:
      res.filename = framePtr.prev.filename
      res.line = framePtr.prev.line
  res.refcount = rcIncrement # refcount is 1  
  sysAssert(isAllocatedPtr(gch.region, res), "newObj: 3")
  when logGC: writeCell("new cell", res)
  gcTrace(res, csAllocated)
  release(gch)
  result = cellToUsr(res)
  zeroMem(result, size)

proc newSeqRC1(typ: PNimType, len: int): pointer {.compilerRtl.} =
  result = newObjRC1(typ, addInt(mulInt(len, typ.base.size), GenericSeqSize))
  cast[PGenericSeq](result).len = len
  cast[PGenericSeq](result).space = len
  
proc growObj(old: pointer, newsize: int, gch: var TGcHeap): pointer =
  acquire(gch)
  collectCT(gch)
  var ol = usrToCell(old)
  sysAssert(ol.typ != nil, "growObj: 1")
  sysAssert(ol.typ.kind in {tyString, tySequence}, "growObj: 2")
  var res = cast[PCell](rawAlloc(gch.region, newsize + sizeof(TCell)))
  var elemSize = 1
  if ol.typ.kind != tyString: elemSize = ol.typ.base.size
  
  var oldsize = cast[PGenericSeq](old).len*elemSize + GenericSeqSize
  copyMem(res, ol, oldsize + sizeof(TCell))
  zeroMem(cast[pointer](cast[TAddress](res)+% oldsize +% sizeof(TCell)),
          newsize-oldsize)
  sysAssert((cast[TAddress](res) and (MemAlign-1)) == 0, "growObj: 3")
  sysAssert(res.refcount shr rcShift <=% 1, "growObj: 4")
  #if res.refcount <% rcIncrement:
  #  add(gch.zct, res)
  #else: # XXX: what to do here?
  #  decRef(ol)
  if (ol.refcount and colorMask) == rcZct:
    var j = gch.zct.len-1
    var d = gch.zct.d
    while j >= 0: 
      if d[j] == ol:
        d[j] = res
        break
      dec(j)
  if canBeCycleRoot(ol): excl(gch.cycleRoots, ol)
  when logGC:
    writeCell("growObj old cell", ol)
    writeCell("growObj new cell", res)
  gcTrace(ol, csZctFreed)
  gcTrace(res, csAllocated)
  when reallyDealloc: rawDealloc(gch.region, ol)
  else:
    sysAssert(ol.typ != nil, "growObj: 5")
    zeroMem(ol, sizeof(TCell))
  release(gch)
  result = cellToUsr(res)

proc growObj(old: pointer, newsize: int): pointer {.rtl.} =
  result = growObj(old, newsize, gch)

# ---------------- cycle collector -------------------------------------------

proc doOperation(p: pointer, op: TWalkOp) =
  if p == nil: return
  var c: PCell = usrToCell(p)
  sysAssert(c != nil, "doOperation: 1")
  case op # faster than function pointers because of easy prediction
  of waZctDecRef:
    sysAssert(c.refcount >=% rcIncrement, "doOperation 2")
    c.refcount = c.refcount -% rcIncrement
    when logGC: writeCell("decref (from doOperation)", c)
    if c.refcount <% rcIncrement: addZCT(gch.zct, c)
  of waPush:
    add(gch.tempStack, c)
  of waCycleDecRef:
    sysAssert(c.refcount >=% rcIncrement, "doOperation 3")
    c.refcount = c.refcount -% rcIncrement

# we now use a much simpler and non-recursive algorithm for cycle removal
proc collectCycles(gch: var TGcHeap) =
  var tabSize = 0
  for c in elements(gch.cycleRoots):
    inc(tabSize)
    forallChildren(c, waCycleDecRef)
  gch.stat.cycleTableSize = max(gch.stat.cycleTableSize, tabSize)

  # restore reference counts (a depth-first traversal is needed):
  var marker: TCellSet
  Init(marker)
  for c in elements(gch.cycleRoots):
    if c.refcount >=% rcIncrement:
      if not containsOrIncl(marker, c):
        gch.tempStack.len = 0
        forAllChildren(c, waPush)
        while gch.tempStack.len > 0:
          dec(gch.tempStack.len)
          var d = gch.tempStack.d[gch.tempStack.len]
          d.refcount = d.refcount +% rcIncrement
          if d in gch.cycleRoots and not containsOrIncl(marker, d):
            forAllChildren(d, waPush)
  # remove cycles:
  for c in elements(gch.cycleRoots):
    if c.refcount <% rcIncrement:
      gch.tempStack.len = 0
      forAllChildren(c, waPush)
      while gch.tempStack.len > 0:
        dec(gch.tempStack.len)
        var d = gch.tempStack.d[gch.tempStack.len]
        if d.refcount <% rcIncrement:
          if d notin gch.cycleRoots: # d is leaf of c and not part of cycle
            addZCT(gch.zct, d)
            when logGC: writeCell("add to ZCT (from cycle collector)", d)
      prepareDealloc(c)
      gcTrace(c, csCycFreed)
      when logGC: writeCell("cycle collector dealloc cell", c)
      when reallyDealloc: rawDealloc(gch.region, c)
      else:
        sysAssert(c.typ != nil, "collectCycles")
        zeroMem(c, sizeof(TCell))
  Deinit(gch.cycleRoots)
  Init(gch.cycleRoots)

proc gcMark(gch: var TGcHeap, p: pointer) {.inline.} =
  # the addresses are not as cells on the stack, so turn them to cells:
  var cell = usrToCell(p)
  var c = cast[TAddress](cell)
  if c >% PageSize and (c and (MemAlign-1)) == 0:
    # fast check: does it look like a cell?
    if isAllocatedPtr(gch.region, cell): 
      # mark the cell:
      cell.refcount = cell.refcount +% rcIncrement
      add(gch.decStack, cell)
    when false:
      # Care for string->cstring and seq->openArray conversions.
      # Now the codegen deals with it, it generated ``nimKeepAlive`` calls.
      var b = cast[PCell](c -% sizeof(TGenericSeq))
      if isAllocatedPtr(gch.region, b):
        # mark the cell:
        b.refcount = b.refcount +% rcIncrement
        add(gch.decStack, b)

proc nimKeepAlive(p: PGenericSeq) {.compilerRtl, noinline.} =
  var c = usrToCell(p)
  if isAllocatedPtr(gch.region, c):
    c.refcount = c.refcount or rcMarked

proc markThreadStacks(gch: var TGcHeap) = 
  when hasThreadSupport and hasSharedHeap:
    {.error: "not fully implemented".}
    var it = threadList
    while it != nil:
      # mark registers: 
      for i in 0 .. high(it.registers): gcMark(gch, it.registers[i])
      var sp = cast[TAddress](it.stackBottom)
      var max = cast[TAddress](it.stackTop)
      # XXX stack direction?
      # XXX unroll this loop:
      while sp <=% max:
        gcMark(gch, cast[ppointer](sp)[])
        sp = sp +% sizeof(pointer)
      it = it.next

# ----------------- stack management --------------------------------------
#  inspired from Smart Eiffel

when defined(sparc):
  const stackIncreases = false
elif defined(hppa) or defined(hp9000) or defined(hp9000s300) or
     defined(hp9000s700) or defined(hp9000s800) or defined(hp9000s820):
  const stackIncreases = true
else:
  const stackIncreases = false

when not defined(useNimRtl):
  proc setStackBottom(theStackBottom: pointer) =
    #c_fprintf(c_stdout, "stack bottom: %p;\n", theStackBottom)
    # the first init must be the one that defines the stack bottom:
    if gch.stackBottom == nil: gch.stackBottom = theStackBottom
    else:
      var a = cast[TAddress](theStackBottom) # and not PageMask - PageSize*2
      var b = cast[TAddress](gch.stackBottom)
      when stackIncreases:
        gch.stackBottom = cast[pointer](min(a, b))
      else:
        gch.stackBottom = cast[pointer](max(a, b))

proc stackSize(): int {.noinline.} =
  var stackTop {.volatile.}: pointer
  result = abs(cast[int](addr(stackTop)) - cast[int](gch.stackBottom))

when defined(sparc): # For SPARC architecture.
  proc isOnStack(p: pointer): bool =
    var stackTop {.volatile.}: pointer
    stackTop = addr(stackTop)
    var b = cast[TAddress](gch.stackBottom)
    var a = cast[TAddress](stackTop)
    var x = cast[TAddress](p)
    result = a <=% x and x <=% b

  proc markStackAndRegisters(gch: var TGcHeap) {.noinline, cdecl.} =
    when defined(sparcv9):
      asm  """"flushw \n" """
    else:
      asm  """"ta      0x3   ! ST_FLUSH_WINDOWS\n" """

    var
      max = gch.stackBottom
      sp: PPointer
      stackTop: array[0..1, pointer]
    sp = addr(stackTop[0])
    # Addresses decrease as the stack grows.
    while sp <= max:
      gcMark(gch, sp[])
      sp = cast[ppointer](cast[TAddress](sp) +% sizeof(pointer))

elif defined(ELATE):
  {.error: "stack marking code is to be written for this architecture".}

elif stackIncreases:
  # ---------------------------------------------------------------------------
  # Generic code for architectures where addresses increase as the stack grows.
  # ---------------------------------------------------------------------------
  proc isOnStack(p: pointer): bool =
    var stackTop {.volatile.}: pointer
    stackTop = addr(stackTop)
    var a = cast[TAddress](gch.stackBottom)
    var b = cast[TAddress](stackTop)
    var x = cast[TAddress](p)
    result = a <=% x and x <=% b

  var
    jmpbufSize {.importc: "sizeof(jmp_buf)", nodecl.}: int
      # a little hack to get the size of a TJmpBuf in the generated C code
      # in a platform independant way

  proc markStackAndRegisters(gch: var TGcHeap) {.noinline, cdecl.} =
    var registers: C_JmpBuf
    if c_setjmp(registers) == 0'i32: # To fill the C stack with registers.
      var max = cast[TAddress](gch.stackBottom)
      var sp = cast[TAddress](addr(registers)) +% jmpbufSize -% sizeof(pointer)
      # sp will traverse the JMP_BUF as well (jmp_buf size is added,
      # otherwise sp would be below the registers structure).
      while sp >=% max:
        gcMark(gch, cast[ppointer](sp)[])
        sp = sp -% sizeof(pointer)

else:
  # ---------------------------------------------------------------------------
  # Generic code for architectures where addresses decrease as the stack grows.
  # ---------------------------------------------------------------------------
  proc isOnStack(p: pointer): bool =
    var stackTop {.volatile.}: pointer
    stackTop = addr(stackTop)
    var b = cast[TAddress](gch.stackBottom)
    var a = cast[TAddress](stackTop)
    var x = cast[TAddress](p)
    result = a <=% x and x <=% b

  proc markStackAndRegisters(gch: var TGcHeap) {.noinline, cdecl.} =
    # We use a jmp_buf buffer that is in the C stack.
    # Used to traverse the stack and registers assuming
    # that 'setjmp' will save registers in the C stack.
    type PStackSlice = ptr array [0..7, pointer]
    var registers: C_JmpBuf
    if c_setjmp(registers) == 0'i32: # To fill the C stack with registers.
      var max = cast[TAddress](gch.stackBottom)
      var sp = cast[TAddress](addr(registers))
      # loop unrolled:
      while sp <% max - 8*sizeof(pointer):
        gcMark(gch, cast[PStackSlice](sp)[0])
        gcMark(gch, cast[PStackSlice](sp)[1])
        gcMark(gch, cast[PStackSlice](sp)[2])
        gcMark(gch, cast[PStackSlice](sp)[3])
        gcMark(gch, cast[PStackSlice](sp)[4])
        gcMark(gch, cast[PStackSlice](sp)[5])
        gcMark(gch, cast[PStackSlice](sp)[6])
        gcMark(gch, cast[PStackSlice](sp)[7])
        sp = sp +% sizeof(pointer)*8
      # last few entries:
      while sp <=% max:
        gcMark(gch, cast[ppointer](sp)[])
        sp = sp +% sizeof(pointer)

# ----------------------------------------------------------------------------
# end of non-portable code
# ----------------------------------------------------------------------------

proc CollectZCT(gch: var TGcHeap) =
  # Note: Freeing may add child objects to the ZCT! So essentially we do 
  # deep freeing, which is bad for incremental operation. In order to 
  # avoid a deep stack, we move objects to keep the ZCT small.
  # This is performance critical!
  var L = addr(gch.zct.len)
  while L[] > 0:
    var c = gch.zct.d[0]
    # remove from ZCT:
    sysAssert((c.refcount and rcZct) == rcZct, "collectZCT")
    c.refcount = c.refcount and not colorMask
    gch.zct.d[0] = gch.zct.d[L[] - 1]
    dec(L[])
    if c.refcount <% rcIncrement: 
      # It may have a RC > 0, if it is in the hardware stack or
      # it has not been removed yet from the ZCT. This is because
      # ``incref`` does not bother to remove the cell from the ZCT 
      # as this might be too slow.
      # In any case, it should be removed from the ZCT. But not
      # freed. **KEEP THIS IN MIND WHEN MAKING THIS INCREMENTAL!**
      if canBeCycleRoot(c): excl(gch.cycleRoots, c)
      when logGC: writeCell("zct dealloc cell", c)
      gcTrace(c, csZctFreed)
      # We are about to free the object, call the finalizer BEFORE its
      # children are deleted as well, because otherwise the finalizer may
      # access invalid memory. This is done by prepareDealloc():
      prepareDealloc(c)
      forAllChildren(c, waZctDecRef)
      when reallyDealloc: rawDealloc(gch.region, c)
      else:
        sysAssert(c.typ != nil, "collectZCT 2")
        zeroMem(c, sizeof(TCell))

proc unmarkStackAndRegisters(gch: var TGcHeap) = 
  var d = gch.decStack.d
  for i in 0..gch.decStack.len-1:
    sysAssert isAllocatedPtr(gch.region, d[i]), "unmarkStackAndRegisters"
    # decRef(d[i]) inlined: cannot create a cycle and must not acquire lock
    var c = d[i]
    # XXX no need for an atomic dec here:
    if --c.refcount:
      addZCT(gch.zct, c)
    sysAssert c.typ != nil, "unmarkStackAndRegisters 2"
  gch.decStack.len = 0

proc collectCT(gch: var TGcHeap) =
  if (gch.zct.len >= ZctThreshold or (cycleGC and
      getOccupiedMem(gch.region) >= gch.cycleThreshold) or stressGC) and 
      gch.recGcLock == 0:
    gch.stat.maxStackSize = max(gch.stat.maxStackSize, stackSize())
    sysAssert(gch.decStack.len == 0, "collectCT")
    markStackAndRegisters(gch)
    markThreadStacks(gch)
    gch.stat.maxStackCells = max(gch.stat.maxStackCells, gch.decStack.len)
    inc(gch.stat.stackScans)
    collectZCT(gch)
    when cycleGC:
      if getOccupiedMem() >= gch.cycleThreshold or stressGC:
        collectCycles(gch)
        collectZCT(gch)
        inc(gch.stat.cycleCollections)
        gch.cycleThreshold = max(InitialCycleThreshold, getOccupiedMem() *
                                 cycleIncrease)
        gch.stat.maxThreshold = max(gch.stat.maxThreshold, gch.cycleThreshold)
    unmarkStackAndRegisters(gch)

when not defined(useNimRtl):
  proc GC_disable() = 
    when hasThreadSupport and hasSharedHeap:
      discard atomicInc(gch.recGcLock, 1)
    else:
      inc(gch.recGcLock)
  proc GC_enable() =
    if gch.recGcLock > 0: 
      when hasThreadSupport and hasSharedHeap:
        discard atomicDec(gch.recGcLock, 1)
      else:
        dec(gch.recGcLock)

  proc GC_setStrategy(strategy: TGC_Strategy) =
    case strategy
    of gcThroughput: nil
    of gcResponsiveness: nil
    of gcOptimizeSpace: nil
    of gcOptimizeTime: nil

  proc GC_enableMarkAndSweep() =
    gch.cycleThreshold = InitialCycleThreshold

  proc GC_disableMarkAndSweep() =
    gch.cycleThreshold = high(gch.cycleThreshold)-1
    # set to the max value to suppress the cycle detector

  proc GC_fullCollect() =
    acquire(gch)
    var oldThreshold = gch.cycleThreshold
    gch.cycleThreshold = 0 # forces cycle collection
    collectCT(gch)
    gch.cycleThreshold = oldThreshold
    release(gch)

  proc GC_getStatistics(): string =
    GC_disable()
    result = "[GC] total memory: " & $(getTotalMem()) & "\n" &
             "[GC] occupied memory: " & $(getOccupiedMem()) & "\n" &
             "[GC] stack scans: " & $gch.stat.stackScans & "\n" &
             "[GC] stack cells: " & $gch.stat.maxStackCells & "\n" &
             "[GC] cycle collections: " & $gch.stat.cycleCollections & "\n" &
             "[GC] max threshold: " & $gch.stat.maxThreshold & "\n" &
             "[GC] zct capacity: " & $gch.zct.cap & "\n" &
             "[GC] max cycle table size: " & $gch.stat.cycleTableSize & "\n" &
             "[GC] max stack size: " & $gch.stat.maxStackSize
    when traceGC: writeLeakage()
    GC_enable()
