#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This file implements the new evaluation engine for Nimrod code.
## An instruction is 1-2 int32s in memory, it is a register based VM.

import
  strutils, ast, astalgo, msgs, vmdef, vmgen, nimsets, types, passes, unsigned,
  parser, vmdeps

from semfold import leValueConv

type
  PStackFrame* = ref TStackFrame
  TStackFrame* = object
    prc: PSym                 # current prc; proc that is evaluated
    slots: TNodeSeq           # parameters passed to the proc + locals;
                              # parameters come first
    next: PStackFrame         # for stacking
    comesFrom: int
    safePoints: seq[int]      # used for exception handling
                              # XXX 'break' should perform cleanup actions
                              # What does the C backend do for it?

proc stackTraceAux(c: PCtx; x: PStackFrame; pc: int) =
  if x != nil:
    stackTraceAux(c, x.next, x.comesFrom)
    var info = c.debug[pc]
    # we now use the same format as in system/except.nim
    var s = toFilename(info)
    var line = toLineNumber(info)
    if line > 0:
      add(s, '(')
      add(s, $line)
      add(s, ')')
    if x.prc != nil:
      for k in 1..max(1, 25-s.len): add(s, ' ')
      add(s, x.prc.name.s)
    MsgWriteln(s)

proc stackTrace(c: PCtx, tos: PStackFrame, pc: int,
                msg: TMsgKind, arg = "") =
  MsgWriteln("stack trace: (most recent call last)")
  stackTraceAux(c, tos, pc)
  LocalError(c.debug[pc], msg, arg)

proc bailOut(c: PCtx; tos: PStackFrame) =
  stackTrace(c, tos, c.exceptionInstr, errUnhandledExceptionX,
             c.currentExceptionA.sons[2].strVal)

when not defined(nimHasInterpreterLoop):
  {.pragma: interpreterLoop.}

template inc(pc: ptr TInstr, diff = 1) =
  inc cast[TAddress](pc), TInstr.sizeof * diff

template ensureKind(k: expr) {.immediate, dirty.} =
  if regs[ra].kind != k:
    myreset(regs[ra])
    regs[ra].kind = k

template decodeB(k: expr) {.immediate, dirty.} =
  let rb = instr.regB
  ensureKind(k)

template decodeBC(k: expr) {.immediate, dirty.} =
  let rb = instr.regB
  let rc = instr.regC
  ensureKind(k)

template declBC() {.immediate, dirty.} =
  let rb = instr.regB
  let rc = instr.regC

template decodeBImm(k: expr) {.immediate, dirty.} =
  let rb = instr.regB
  let imm = instr.regC - byteExcess
  ensureKind(k)

template decodeBx(k: expr) {.immediate, dirty.} =
  let rbx = instr.regBx - wordExcess
  ensureKind(k)

proc myreset(n: PNode) =
  when defined(system.reset): 
    var oldInfo = n.info
    reset(n[])
    n.info = oldInfo

template move(a, b: expr) = system.shallowCopy(a, b)
# XXX fix minor 'shallowCopy' overloading bug in compiler

proc asgnRef(x, y: PNode) =
  myreset(x)
  x.kind = y.kind
  x.typ = y.typ
  case x.kind
  of nkCharLit..nkInt64Lit: x.intVal = y.intVal
  of nkFloatLit..nkFloat64Lit: x.floatVal = y.floatVal
  of nkStrLit..nkTripleStrLit: x.strVal = y.strVal
  of nkIdent: x.ident = y.ident
  of nkSym: x.sym = y.sym
  else:
    if x.kind notin {nkEmpty..nkNilLit}:
      move(x.sons, y.sons)

proc asgnComplex(x, y: PNode) =
  myreset(x)
  x.kind = y.kind
  x.typ = y.typ
  case x.kind
  of nkCharLit..nkInt64Lit: x.intVal = y.intVal
  of nkFloatLit..nkFloat64Lit: x.floatVal = y.floatVal
  of nkStrLit..nkTripleStrLit: x.strVal = y.strVal
  of nkIdent: x.ident = y.ident
  of nkSym: x.sym = y.sym
  else:
    if x.kind notin {nkEmpty..nkNilLit}:
      let y = y.copyTree
      for i in countup(0, sonsLen(y) - 1): addSon(x, y.sons[i])

template getstr(a: expr): expr =
  (if a.kind == nkStrLit: a.strVal else: $chr(int(a.intVal)))

proc pushSafePoint(f: PStackFrame; pc: int) =
  if f.safePoints.isNil: f.safePoints = @[]
  f.safePoints.add(pc)

proc popSafePoint(f: PStackFrame) = discard f.safePoints.pop()

proc cleanUpOnException(c: PCtx; tos: PStackFrame; regs: TNodeSeq): int =
  let raisedType = c.currentExceptionA.typ.skipTypes(abstractPtrs)
  var f = tos
  while true:
    while f.safePoints.isNil or f.safePoints.len == 0:
      f = f.next
      if f.isNil: return -1
    var pc2 = f.safePoints[f.safePoints.high]

    var nextExceptOrFinally = -1
    if c.code[pc2].opcode == opcExcept:
      nextExceptOrFinally = pc2 + c.code[pc2].regBx - wordExcess
      inc pc2
    while c.code[pc2].opcode == opcExcept:
      let exceptType = c.types[c.code[pc2].regBx-wordExcess].skipTypes(
                          abstractPtrs)
      if inheritanceDiff(exceptType, raisedType) <= 0:
        # mark exception as handled but keep it in B for 
        # the getCurrentException() builtin:
        c.currentExceptionB = c.currentExceptionA
        c.currentExceptionA = nil
        # execute the corresponding handler:
        return pc2
      inc pc2
    if nextExceptOrFinally >= 0:
      pc2 = nextExceptOrFinally
    if c.code[pc2].opcode == opcFinally:
      # execute the corresponding handler, but don't quit walking the stack:
      return pc2
    # not the right one:
    discard f.safePoints.pop

proc cleanUpOnReturn(c: PCtx; f: PStackFrame): int =
  if f.safePoints.isNil: return -1
  for s in f.safePoints:
    var pc = s
    while c.code[pc].opcode == opcExcept:
      pc = pc + c.code[pc].regBx - wordExcess
    if c.code[pc].opcode == opcFinally:
      return pc
  return -1

proc compile(c: PCtx, s: PSym): int = 
  result = vmgen.genProc(c, s)
  #c.echoCode

proc execute(c: PCtx, start: int) =
  var pc = start
  var regs: TNodeSeq # alias to tos.slots for performance
  var tos: PStackFrame
  newSeq(regs, c.prc.maxSlots)
  while true:
    {.interpreterLoop.}
    let instr = c.code[pc]
    let ra = instr.regA
    #echo "PC ", pc, " ", c.code[pc].opcode, " ra ", ra
    case instr.opcode
    of opcEof: break
    of opcRet:
      # XXX perform any cleanup actions
      pc = tos.comesFrom
      tos = tos.next
      if tos.isNil: return
      
      let retVal = regs[0]
      move(regs, tos.slots)
      assert c.code[pc].opcode in {opcIndCall, opcIndCallAsgn}
      if c.code[pc].opcode == opcIndCallAsgn:
        regs[c.code[pc].regA] = retVal
    of opcYldYoid: assert false
    of opcYldVal: assert false
    of opcAsgnInt:
      decodeB(nkIntLit)
      regs[ra].intVal = regs[rb].intVal
    of opcAsgnStr:
      decodeB(nkStrLit)
      regs[ra].strVal = regs[rb].strVal
    of opcAsgnFloat:
      decodeB(nkFloatLit)
      regs[ra].floatVal = regs[rb].floatVal
    of opcAsgnComplex:
      asgnComplex(regs[ra], regs[instr.regB])
    of opcAsgnRef:
      asgnRef(regs[ra], regs[instr.regB])
    of opcWrGlobalRef:
      asgnRef(c.globals[instr.regBx-wordExcess-1], regs[ra])
    of opcWrGlobal:
      asgnComplex(c.globals.sons[instr.regBx-wordExcess-1], regs[ra])
    of opcLdArr:
      # a = b[c]
      let rb = instr.regB
      let rc = instr.regC
      let idx = regs[rc].intVal
      # XXX what if the array is not 0-based? -> codegen should insert a sub
      regs[ra] = regs[rb].sons[idx.int]
    of opcLdStrIdx:
      decodeBC(nkIntLit)
      let idx = regs[rc].intVal
      regs[ra].intVal = regs[rb].strVal[idx.int].ord
    of opcWrArr:
      # a[b] = c
      let rb = instr.regB
      let rc = instr.regC
      let idx = regs[rb].intVal
      asgnComplex(regs[ra].sons[idx.int], regs[rc])
    of opcWrArrRef:
      let rb = instr.regB
      let rc = instr.regC
      let idx = regs[rb].intVal
      asgnRef(regs[ra].sons[idx.int], regs[rc])
    of opcLdObj:
      # a = b.c
      let rb = instr.regB
      let rc = instr.regC
      # XXX this creates a wrong alias
      asgnComplex(regs[ra], regs[rb].sons[rc])
    of opcWrObj:
      # a.b = c
      let rb = instr.regB
      let rc = instr.regC
      asgnComplex(regs[ra].sons[rb], regs[rc])
    of opcWrObjRef:
      let rb = instr.regB
      let rc = instr.regC
      asgnRef(regs[ra].sons[rb], regs[rc])
    of opcWrStrIdx:
      decodeBC(nkStrLit)
      let idx = regs[rb].intVal.int
      regs[ra].strVal[idx] = chr(regs[rc].intVal)
    of opcAddr:
      decodeB(nkRefTy)
      if regs[ra].len == 0: regs[ra].add regs[rb]
      else: regs[ra].sons[0] = regs[rb]
    of opcDeref:
      # a = b[]
      let rb = instr.regB
      if regs[rb].kind == nkNilLit:
        stackTrace(c, tos, pc, errNilAccess)
      assert regs[rb].kind == nkRefTy
      regs[ra] = regs[rb].sons[0]
    of opcAddInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal + regs[rc].intVal
    of opcAddImmInt:
      decodeBImm(nkIntLit)
      regs[ra].intVal = regs[rb].intVal + imm
    of opcSubInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal - regs[rc].intVal
    of opcSubImmInt:
      decodeBImm(nkIntLit)
      regs[ra].intVal = regs[rb].intVal - imm
    of opcLenSeq:
      decodeBImm(nkIntLit)
      #assert regs[rb].kind == nkBracket
      # also used by mNLen
      regs[ra].intVal = regs[rb].len - imm
    of opcLenStr:
      decodeBImm(nkIntLit)
      assert regs[rb].kind == nkStrLit
      regs[ra].intVal = regs[rb].strVal.len - imm
    of opcIncl:
      decodeB(nkCurly)
      if not inSet(regs[ra], regs[rb]): addSon(regs[ra], copyTree(regs[rb]))
    of opcExcl:
      decodeB(nkCurly)
      # XXX arg we need types here :-(
      var b = newNodeIT(nkCurly, regs[rb].info, regs[rb].typ)
      addSon(b, regs[rb])
      var r = diffSets(regs[ra], b)
      discardSons(regs[ra])
      for i in countup(0, sonsLen(r) - 1): addSon(regs[ra], r.sons[i])
    of opcCard:
      decodeB(nkIntLit)
      regs[ra].intVal = nimsets.cardSet(regs[rb])
    of opcMulInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal * regs[rc].intVal
    of opcDivInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal div regs[rc].intVal
    of opcModInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal mod regs[rc].intVal
    of opcAddFloat:
      decodeBC(nkFloatLit)
      regs[ra].floatVal = regs[rb].floatVal + regs[rc].floatVal
    of opcSubFloat:
      decodeBC(nkFloatLit)
      regs[ra].floatVal = regs[rb].floatVal - regs[rc].floatVal
    of opcMulFloat:
      decodeBC(nkFloatLit)
      regs[ra].floatVal = regs[rb].floatVal * regs[rc].floatVal
    of opcDivFloat:
      decodeBC(nkFloatLit)
      regs[ra].floatVal = regs[rb].floatVal / regs[rc].floatVal
    of opcShrInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal shr regs[rc].intVal
    of opcShlInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal shl regs[rc].intVal
    of opcBitandInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal and regs[rc].intVal
    of opcBitorInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal or regs[rc].intVal
    of opcBitxorInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal xor regs[rc].intVal
    of opcAddu:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal +% regs[rc].intVal
    of opcSubu:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal -% regs[rc].intVal
    of opcMulu: 
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal *% regs[rc].intVal
    of opcDivu:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal /% regs[rc].intVal
    of opcModu:
      decodeBC(nkIntLit)
      regs[ra].intVal = regs[rb].intVal %% regs[rc].intVal
    of opcEqInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal == regs[rc].intVal)
    of opcLeInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal <= regs[rc].intVal)
    of opcLtInt:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal < regs[rc].intVal)
    of opcEqFloat:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].floatVal == regs[rc].floatVal)
    of opcLeFloat:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].floatVal <= regs[rc].floatVal)
    of opcLtFloat:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].floatVal < regs[rc].floatVal)
    of opcLeu:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal <=% regs[rc].intVal)
    of opcLtu:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal <% regs[rc].intVal)
    of opcEqRef:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb] == regs[rc])
      # XXX is this correct? nope ...
    of opcXor:
      decodeBC(nkIntLit)
      regs[ra].intVal = ord(regs[rb].intVal != regs[rc].intVal)
    of opcNot:
      decodeB(nkIntLit)
      assert regs[rb].kind == nkIntLit
      regs[ra].intVal = 1 - regs[rb].intVal
    of opcUnaryMinusInt:
      decodeB(nkIntLit)
      assert regs[rb].kind == nkIntLit
      regs[ra].intVal = -regs[rb].intVal
    of opcUnaryMinusFloat:
      decodeB(nkFloatLit)
      assert regs[rb].kind == nkFloatLit
      regs[ra].floatVal = -regs[rb].floatVal
    of opcBitnotInt:
      decodeB(nkIntLit)
      assert regs[rb].kind == nkIntLit
      regs[ra].intVal = not regs[rb].intVal
    of opcEqStr:
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(regs[rb].strVal == regs[rc].strVal)
    of opcLeStr:
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(regs[rb].strVal <= regs[rc].strVal)
    of opcLtStr:
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(regs[rb].strVal < regs[rc].strVal)
    of opcLeSet:
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(containsSets(regs[rb], regs[rc]))
    of opcEqSet: 
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(equalSets(regs[rb], regs[rc]))
    of opcLtSet:
      decodeBC(nkIntLit)
      let a = regs[rb]
      let b = regs[rc]
      regs[ra].intVal = Ord(containsSets(a, b) and not equalSets(a, b))
    of opcMulSet:
      decodeBC(nkCurly)
      move(regs[ra].sons, nimsets.intersectSets(regs[rb], regs[rc]).sons)
    of opcPlusSet: 
      decodeBC(nkCurly)
      move(regs[ra].sons, nimsets.unionSets(regs[rb], regs[rc]).sons)
    of opcMinusSet:
      decodeBC(nkCurly)
      move(regs[ra].sons, nimsets.diffSets(regs[rb], regs[rc]).sons)
    of opcSymDiffSet:
      decodeBC(nkCurly)
      move(regs[ra].sons, nimsets.symdiffSets(regs[rb], regs[rc]).sons)    
    of opcConcatStr:
      decodeBC(nkStrLit)
      regs[ra].strVal = getstr(regs[rb])
      for i in rb+1..rb+rc-1:
        regs[ra].strVal.add getstr(regs[i])
    of opcAddStrCh:
      decodeB(nkStrLit)
      regs[ra].strVal.add(regs[rb].intVal.chr)
    of opcAddStrStr:
      decodeB(nkStrLit)
      regs[ra].strVal.add(regs[rb].strVal)
    of opcAddSeqElem:
      decodeB(nkBracket)
      regs[ra].add(copyTree(regs[rb]))
    of opcEcho:
      echo regs[ra].strVal
    of opcContainsSet:
      decodeBC(nkIntLit)
      regs[ra].intVal = Ord(inSet(regs[rb], regs[rc]))
    of opcSubStr:
      decodeBC(nkStrLit)
      inc pc
      assert c.code[pc].opcode == opcSubStr
      let rd = c.code[pc].regA
      regs[ra].strVal = substr(regs[rb].strVal, regs[rc].intVal.int, 
                               regs[rd].intVal.int)
    of opcRangeChck:
      let rb = instr.regB
      let rc = instr.regC
      if not (leValueConv(regs[rb], regs[ra]) and
              leValueConv(regs[ra], regs[rc])):
        stackTrace(c, tos, pc, errGenerated,
          msgKindToString(errIllegalConvFromXtoY) % [
          "unknown type" , "unknown type"])
    of opcIndCall, opcIndCallAsgn:
      # dest = call regStart, n; where regStart = fn, arg1, ...
      let rb = instr.regB
      let rc = instr.regC
      let prc = regs[rb].sym
      let newPc = compile(c, prc)
      var newFrame = PStackFrame(prc: prc, comesFrom: pc, next: tos)
      newSeq(newFrame.slots, prc.position)
      if not isEmptyType(prc.typ.sons[0]):
        newFrame.slots[0] = getNullValue(prc.typ.sons[0], prc.info)
      # pass every parameter by var (the language definition allows this):
      for i in 1 .. rc-1:
        newFrame.slots[i] = regs[rb+i]
      # allocate the temporaries:
      for i in rc .. <prc.position:
        newFrame.slots[i] = newNode(nkEmpty)
      tos = newFrame
      move(regs, newFrame.slots)
      # -1 for the following 'inc pc'
      pc = newPc-1
    of opcTJmp:
      # jump Bx if A != 0
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      if regs[ra].intVal != 0:
        inc pc, rbx
    of opcFJmp:
      # jump Bx if A == 0
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      if regs[ra].intVal == 0:
        inc pc, rbx
    of opcJmp:
      # jump Bx
      let rbx = instr.regBx - wordExcess - 1 # -1 for the following 'inc pc'
      inc pc, rbx
    of opcBranch:
      # we know the next instruction is a 'jmp':
      let branch = c.constants[instr.regBx-wordExcess]
      var cond = false
      for j in countup(0, sonsLen(branch) - 2): 
        if overlap(regs[ra], branch.sons[j]): 
          cond = true
          break
      assert c.code[pc+1].opcode == opcJmp
      inc pc 
      # we skip this instruction so that the final 'inc(pc)' skips
      # the following jump
      if cond:
        let instr2 = c.code[pc]
        let rbx = instr2.regBx - wordExcess - 1 # -1 for the following 'inc pc'
        inc pc, rbx
    of opcTry:
      let rbx = instr.regBx - wordExcess
      tos.pushSafePoint(pc + rbx)
    of opcExcept:
      # just skip it; it's followed by a jump;
      # we'll execute in the 'raise' handler
      discard
    of opcFinally:
      # just skip it; it's followed by the code we need to execute anyway
      tos.popSafePoint()
    of opcFinallyEnd:
      if c.currentExceptionA != nil:
        # we are in a cleanup run:
        pc = cleanupOnException(c, tos, regs)-1
        if pc < 0: 
          bailOut(c, tos)
          return
    of opcRaise:
      let raised = regs[ra]
      c.currentExceptionA = raised
      c.exceptionInstr = pc
      # -1 because of the following 'inc'
      pc = cleanupOnException(c, tos, regs) - 1
      if pc < 0:
        bailOut(c, tos)
        return
    of opcNew:
      let typ = c.types[instr.regBx - wordExcess]
      regs[ra] = getNullValue(typ, regs[ra].info)
    of opcNewSeq:
      let typ = c.types[instr.regBx - wordExcess]
      inc pc
      ensureKind(nkBracket)
      let instr2 = c.code[pc]
      let rb = instr2.regA
      regs[ra].typ = typ
      newSeq(regs[ra].sons, rb)
      for i in 0 .. <rb:
        regs[ra].sons[i] = getNullValue(typ, regs[ra].info)
    of opcNewStr:
      decodeB(nkStrLit)
      regs[ra].strVal = newString(regs[rb].intVal.int)
    of opcLdImmInt:
      # dest = immediate value
      decodeBx(nkIntLit)
      regs[ra].intVal = rbx
    of opcLdNull:
      let typ = c.types[instr.regBx - wordExcess]
      regs[ra] = getNullValue(typ, c.debug[pc])
    of opcLdConst:
      regs[ra] = c.constants.sons[instr.regBx - wordExcess]
    of opcAsgnConst:
      let rb = instr.regBx - wordExcess
      if regs[ra].isNil:
        regs[ra] = copyTree(c.constants.sons[rb])
      else:
        asgnComplex(regs[ra], c.constants.sons[rb])
    of opcNBindSym:
      # trivial implementation:
      let rb = instr.regB
      regs[ra] = regs[rb].sons[1]
    of opcNChild:
      let rb = instr.regB
      let rc = instr.regC
      regs[ra] = regs[rb].sons[regs[rc].intVal.int]
    of opcNSetChild:
      let rb = instr.regB
      let rc = instr.regC
      regs[ra].sons[regs[rb].intVal.int] = regs[rc]
    of opcNAdd:
      declBC()
      regs[rb].add(regs[rb])
      regs[ra] = regs[rb]
    of opcNAddMultiple:
      declBC()
      let x = regs[rc]
      # XXX can be optimized:
      for i in 0.. <x.len: regs[rb].add(x.sons[i])
      regs[ra] = regs[rb]
    of opcNKind:
      decodeB(nkIntLit)
      regs[ra].intVal = ord(regs[rb].kind)
    of opcNIntVal:
      decodeB(nkIntLit)
      let a = regs[rb]
      case a.kind
      of nkCharLit..nkInt64Lit: regs[ra].intVal = a.intVal
      else: stackTrace(c, tos, pc, errFieldXNotFound, "intVal")
    of opcNFloatVal:
      decodeB(nkFloatLit)
      let a = regs[rb]
      case a.kind
      of nkFloatLit..nkFloat64Lit: regs[ra].floatVal = a.floatVal
      else: stackTrace(c, tos, pc, errFieldXNotFound, "floatVal")
    of opcNSymbol:
      let rb = instr.regB
      if regs[rb].kind != nkSym: 
        stackTrace(c, tos, pc, errFieldXNotFound, "symbol")
      regs[ra] = regs[rb]
    of opcNIdent:
      let rb = instr.regB
      if regs[rb].kind != nkIdent: 
        stackTrace(c, tos, pc, errFieldXNotFound, "ident")
      regs[ra] = regs[rb]
    of opcNGetType:
      InternalError(c.debug[pc], "unknown opcode " & $instr.opcode)      
    of opcNStrVal:
      decodeB(nkStrLit)
      let a = regs[rb]
      case a.kind
      of nkStrLit..nkTripleStrLit: regs[ra].strVal = a.strVal
      else: stackTrace(c, tos, pc, errFieldXNotFound, "strVal")
    of opcSlurp:
      decodeB(nkStrLit)
      regs[ra].strVal = opSlurp(regs[rb].strVal, c.debug[pc], c.module)
    of opcGorge:
      decodeBC(nkStrLit)
      regs[ra].strVal = opGorge(regs[rb].strVal, regs[rc].strVal)
    of opcNError:
      stackTrace(c, tos, pc, errUser, regs[ra].strVal)
    of opcNWarning:
      Message(c.debug[pc], warnUser, regs[ra].strVal)
    of opcNHint:
      Message(c.debug[pc], hintUser, regs[ra].strVal)
    of opcParseExprToAst:
      let rb = instr.regB
      # c.debug[pc].line.int - countLines(regs[rb].strVal) ?
      let ast = parseString(regs[rb].strVal, c.debug[pc].toFilename,
                            c.debug[pc].line.int)
      if sonsLen(ast) != 1:
        GlobalError(c.debug[pc], errExprExpected, "multiple statements")
      regs[ra] = ast.sons[0]
    of opcParseStmtToAst:
      let rb = instr.regB
      let ast = parseString(regs[rb].strVal, c.debug[pc].toFilename,
                            c.debug[pc].line.int)
      regs[ra] = ast
    of opcCallSite:
      if c.callsite != nil: regs[ra] = c.callsite
      else: stackTrace(c, tos, pc, errFieldXNotFound, "callsite")
    else:
      InternalError(c.debug[pc], "unknown opcode " & $instr.opcode)
    inc pc

proc eval*(c: PCtx, n: PNode): PNode =
  ## eval never returns nil! This simplifies the code a lot and
  ## makes it faster too.
  let start = genStmt(c, n)
  # execute new instructions; this redundant opcEof check saves us lots
  # of allocations in 'execute':
  if c.code[start].opcode != opcEof:
    execute(c, start)
  result = emptyNode

proc myOpen(module: PSym): PPassContext =
  #var c = newEvalContext(module, emRepl)
  #c.features = {allowCast, allowFFI, allowInfiniteLoops}
  #pushStackFrame(c, newStackFrame())
  result = newCtx(module)

var oldErrorCount: int

proc myProcess(c: PPassContext, n: PNode): PNode =
  # don't eval errornous code:
  if oldErrorCount == msgs.gErrorCounter:
    result = eval(PCtx(c), n)
  else:
    result = n
  oldErrorCount = msgs.gErrorCounter

const vmPass* = makePass(myOpen, nil, myProcess, myProcess)

