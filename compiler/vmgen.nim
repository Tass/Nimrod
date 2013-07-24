#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the code generator for the VM.

import
  unsigned, strutils, ast, astalgo, types, msgs, renderer, vmdef, 
  trees, intsets, rodread

proc codeListing(c: PCtx, result: var string) =
  # for debugging purposes
  var i = 0
  while i < c.code.len:
    if i in c.jumpTargets: result.addf("L$1:\n", i)
    let x = c.code[i]

    let opc = opcode(x)
    if opc < firstABxInstr:
      result.addf("\t$#\tr$#, r$#, r$#", ($opc).substr(3), x.regA, 
                  x.regB, x.regC)
    else:
      result.addf("\t$#\tr$#, r$#", ($opc).substr(3), x.regA, x.regBx)
    result.add(" #")
    result.add(toFileLine(c.debug[i]))
    result.add("\n")
    inc i

proc gABC(ctx: PCtx; n: PNode; opc: TOpcode; a, b, c: TRegister = 0) =
  assert opc.ord < 255
  let ins = (opc.uint32 or (a.uint32 shl 8'u32) or
                           (b.uint32 shl 16'u32) or
                           (c.uint32 shl 24'u32)).TInstr
  ctx.code.add(ins)
  ctx.debug.add(n.info)

proc gABI(c: PCtx; n: PNode; opc: TOpcode; a, b: TRegister; imm: biggestInt) =
  let ins = (opc.ord or a shl 8 or b shl 16 or (imm+byteExcess) shl 24).TInstr
  c.code.add(ins)
  c.debug.add(n.info)

proc gABx(c: PCtx; n: PNode; opc: TOpcode; a: TRegister = 0; bx: int) =
  let ins = (opc.ord or a shl 8 or (bx+wordExcess) shl 16).TInstr
  c.code.add(ins)
  c.debug.add(n.info)

proc xjmp(c: PCtx; n: PNode; opc: TOpcode; a: TRegister = 0): TPosition =
  #assert opc in {opcJmp, opcFJmp, opcTJmp}
  gABx(c, n, opc, a, 0)
  result = TPosition(c.code.len)

proc genLabel(c: PCtx): TPosition =
  result = TPosition(c.code.len)
  c.jumpTargets.incl(c.code.len)

proc jmp(c: PCtx, n: PNode, opc: TOpcode, p = TPosition(0)) =
  let dist = p.int - c.code.len
  InternalAssert(-0x7fff < dist and dist < 0x7fff)
  gABx(c, n, opc, 0, dist)

proc patch(c: PCtx, p: TPosition) =
  # patch with current index
  let p = p.int
  let diff = c.code.len - p
  c.jumpTargets.incl(c.code.len)
  InternalAssert(-0x7fff < diff and diff < 0x7fff)
  let oldInstr = c.code[p]
  # opcode and regA stay the same:
  c.code[p] = ((oldInstr.int and 0xffff) or (diff+wordExcess)).TInstr

proc getSlotKind(t: PType): TSlotKind =
  case t.skipTypes(abstractRange).kind
  of tyBool, tyChar, tyEnum, tyOrdinal, tyInt..tyInt64, tyUInt..tyUInt64:
    slotTempInt
  of tyString, tyCString:
    slotTempStr
  of tyFloat..tyFloat128:
    slotTempFloat
  else:
    slotTempComplex

const
  HighRegisterPressure = 40

proc getTemp(c: PCtx; typ: PType): TRegister =
  let c = c.prc
  # we prefer the same slot kind here for efficiency:
  let k = typ.getSlotKind
  for i in 0 .. c.maxSlots-1:
    if c.slots[i].kind == k and not c.slots[i].inUse:
      c.slots[i].inUse = true
      return TRegister(i)
      
  # if register pressure is high, we re-use more aggressively:
  if c.maxSlots >= HighRegisterPressure:
    for i in 0 .. c.maxSlots-1:
      if not c.slots[i].inUse:
        c.slots[i] = (inUse: true, kind: k)
        return TRegister(i)
  result = TRegister(c.maxSlots)
  c.slots[c.maxSlots] = (inUse: true, kind: k)
  inc c.maxSlots

proc freeTemp(c: PCtx; r: TRegister) =
  let c = c.prc
  if c.slots[r].kind >= slotSomeTemp: c.slots[r].inUse = false

proc getTempRange(c: PCtx; n: int; kind: TSlotKind): TRegister =
  # if register pressure is high, we re-use more aggressively:
  let c = c.prc
  if c.maxSlots >= HighRegisterPressure or c.maxSlots+n >= high(TRegister):
    for i in 0 .. c.maxSlots-1:
      block search:
        if not c.slots[i].inUse:
          for j in i+1 .. i+n-1:
            if c.slots[j].inUse: break search
        result = TRegister(i)
        for k in result .. result+n-1: c.slots[k] = (inUse: true, kind: kind)
        return
  if c.maxSlots+n >= high(TRegister):
    InternalError("cannot generate code; too many registers required")
  result = TRegister(c.maxSlots)
  inc c.maxSlots, n
  for k in result .. result+n-1: c.slots[k] = (inUse: true, kind: kind)
  
proc freeTempRange(c: PCtx; start: TRegister, n: int) =
  for i in start .. start+n-1: c.freeTemp(TRegister(i))

template withTemp(tmp, typ: expr, body: stmt) {.immediate, dirty.} =
  var tmp = getTemp(c, typ)
  body
  c.freeTemp(tmp)

proc popBlock(c: PCtx; oldLen: int) =  
  for f in c.prc.blocks[oldLen].fixups:
    c.patch(f)
  c.prc.blocks.setLen(oldLen)

template withBlock(labl: PSym; body: stmt) {.immediate, dirty.} =
  var oldLen {.gensym.} = c.prc.blocks.len
  c.prc.blocks.add TBlock(label: labl, fixups: @[])
  body
  popBlock(c, oldLen)

proc gen(c: PCtx; n: PNode; dest: var TDest)
proc gen(c: PCtx; n: PNode; dest: TRegister) =
  var d: TDest = dest
  gen(c, n, d)
  InternalAssert d == dest

proc gen(c: PCtx; n: PNode) =
  var tmp: TDest = -1
  gen(c, n, tmp)
  InternalAssert tmp < 0

proc genx(c: PCtx; n: PNode): TRegister =
  var tmp: TDest = -1
  gen(c, n, tmp)
  result = TRegister(tmp)

proc genWhile(c: PCtx; n: PNode) =
  # L1:
  #   cond, tmp
  #   fjmp tmp, L2
  #   body
  #   jmp L1
  # L2:
  let L1 = c.genLabel
  withBlock(nil):
    var tmp = c.genx(n.sons[0])
    let L2 = c.xjmp(n, opcFJmp, tmp)
    c.freeTemp(tmp)
    c.gen(n.sons[1])
    c.jmp(n, opcJmp, L1)
    c.patch(L2)

proc genBlock(c: PCtx; n: PNode; dest: var TDest) =
  withBlock(n.sons[0].sym):
    c.gen(n.sons[1], dest)

proc genBreak(c: PCtx; n: PNode) =
  let L1 = c.xjmp(n, opcJmp)
  if n.sons[0].kind == nkSym:
    for i in countdown(c.prc.blocks.len-1, 0):
      if c.prc.blocks[i].label == n.sons[0].sym:
        c.prc.blocks[i].fixups.add L1
        break
    InternalError(n.info, "cannot find 'break' target")
  else:
    c.prc.blocks[c.prc.blocks.high].fixups.add L1

proc genIf(c: PCtx, n: PNode; dest: var TDest) =
  #  if (!expr1) goto L1;
  #    thenPart
  #    goto LEnd
  #  L1:
  #  if (!expr2) goto L2;
  #    thenPart2
  #    goto LEnd
  #  L2:
  #    elsePart
  #  Lend:
  if dest < 0 and not isEmptyType(n.typ): dest = getTemp(c, n.typ)
  var endings: seq[TPosition] = @[]
  for i in countup(0, len(n) - 1):
    var it = n.sons[i]
    if it.len == 2:
      withTemp(tmp, it.sons[0].typ):
        c.gen(it.sons[0], tmp)
        let elsePos = c.xjmp(it.sons[0], opcFJmp, tmp) # if false
      c.gen(n.sons[1], dest) # then part
      if i < sonsLen(n)-1:
        endings.add(c.xjmp(it.sons[1], opcJmp, 0))
      c.patch(elsePos)
    else:
      c.gen(it.sons[0], dest)
  for endPos in endings: c.patch(endPos)

proc genAndOr(c: PCtx; n: PNode; opc: TOpcode; dest: var TDest) =
  #   asgn dest, a
  #   tjmp|fjmp L1
  #   asgn dest, b
  # L1:
  if dest < 0: dest = getTemp(c, n.typ)
  c.gen(n.sons[0], dest)
  let L1 = c.xjmp(n, opc)
  c.gen(n.sons[1], dest)
  c.patch(L1)

proc rawGenLiteral(c: PCtx; n: PNode): int =
  result = c.constants.len
  c.constants.add n
  InternalAssert result < 0x7fff

proc sameConstant*(a, b: PNode): bool =
  result = false
  if a == b:
    result = true
  elif a != nil and b != nil and a.kind == b.kind:
    case a.kind
    of nkSym: result = a.sym == b.sym
    of nkIdent: result = a.ident.id == b.ident.id
    of nkCharLit..nkInt64Lit: result = a.intVal == b.intVal
    of nkFloatLit..nkFloat64Lit: result = a.floatVal == b.floatVal
    of nkStrLit..nkTripleStrLit: result = a.strVal == b.strVal
    of nkEmpty, nkNilLit, nkType: result = true
    else: 
      if sonsLen(a) == sonsLen(b): 
        for i in countup(0, sonsLen(a) - 1): 
          if not sameConstant(a.sons[i], b.sons[i]): return 
        result = true

proc genLiteral(c: PCtx; n: PNode): int =
  # types do not matter here:
  for i in 0 .. <c.constants.len:
    if sameConstant(c.constants[i], n): return i
  result = rawGenLiteral(c, n)

proc genCase(c: PCtx; n: PNode; dest: var TDest) =
  #  if (!expr1) goto L1;
  #    thenPart
  #    goto LEnd
  #  L1:
  #  if (!expr2) goto L2;
  #    thenPart2
  #    goto LEnd
  #  L2:
  #    elsePart
  #  Lend:
  if dest < 0 and not isEmptyType(n.typ): dest = getTemp(c, n.typ)
  var endings: seq[TPosition] = @[]
  withTemp(tmp, n.sons[0].typ):
    c.gen(n.sons[0], tmp)
    # branch tmp, codeIdx
    # fjmp   elseLabel
    for i in 1 .. <n.len:
      let it = n.sons[i]
      if it.len == 1:
        # else stmt:
        c.gen(it.sons[0], dest)
      else:
        let b = rawGenLiteral(c, it)
        c.gABx(it, opcBranch, tmp, b)
        let elsePos = c.xjmp(it.lastSon, opcFJmp, tmp)
        c.gen(it.lastSon, dest)
        if i < sonsLen(n)-1:
          endings.add(c.xjmp(it.lastSon, opcJmp, 0))
        c.patch(elsePos)
  for endPos in endings: c.patch(endPos)

proc genType(c: PCtx; typ: PType): int =
  for i, t in c.types:
    if sameType(t, typ): return i
  result = c.types.len
  c.types.add(typ)
  internalAssert(result <= 0x7fff)

proc genTry(c: PCtx; n: PNode; dest: var TDest) =
  if dest < 0 and not isEmptyType(n.typ): dest = getTemp(c, n.typ)
  var endings: seq[TPosition] = @[]
  let elsePos = c.xjmp(n, opcTry, 0)
  c.gen(n.sons[0], dest)
  c.patch(elsePos)
  for i in 1 .. <n.len:
    let it = n.sons[i]
    if it.kind != nkFinally:
      var blen = len(it)
      # first opcExcept contains the end label of the 'except' block:
      let endExcept = c.xjmp(it, opcExcept, 0)
      for j in countup(0, blen - 2): 
        assert(it.sons[j].kind == nkType)
        let typ = it.sons[j].typ.skipTypes(abstractPtrs)
        c.gABx(it, opcExcept, 0, c.genType(typ))
      if blen == 1: 
        # general except section:
        c.gABx(it, opcExcept, 0, 0)
      c.gen(it.lastSon, dest)
      if i < sonsLen(n)-1:
        endings.add(c.xjmp(it, opcJmp, 0))
      c.patch(endExcept)
  for endPos in endings: c.patch(endPos)
  let fin = lastSon(n)
  # we always generate an 'opcFinally' as that pops the safepoint
  # from the stack
  c.gABx(fin, opcFinally, 0, 0)
  if fin.kind == nkFinally:
    c.gen(fin.sons[0], dest)
  c.gABx(fin, opcFinallyEnd, 0, 0)

proc genRaise(c: PCtx; n: PNode) =
  let dest = genx(c, n.sons[0])
  c.gABC(n, opcRaise, dest)
  c.freeTemp(dest)

proc genReturn(c: PCtx; n: PNode) =
  if n.sons[0].kind != nkEmpty:
    gen(c, n.sons[0])
  c.gABC(n, opcRet)

proc genCall(c: PCtx; n: PNode; dest: var TDest) =
  if dest < 0 and not isEmptyType(n.typ): dest = getTemp(c, n.typ)
  let x = c.getTempRange(n.len, slotTempUnknown)
  for i in 0.. <n.len: 
    var r: TRegister = x+i
    c.gen(n.sons[i], r)
  if dest < 0:
    c.gABC(n, opcIndCall, 0, x, n.len)
  else:
    c.gABC(n, opcIndCallAsgn, dest, x, n.len)
  c.freeTempRange(x, n.len)

proc genNew(c: PCtx; n: PNode) =
  let dest = c.genx(n.sons[1])
  c.gABx(n, opcNew, dest, c.genType(n.sons[1].typ.skipTypes(abstractVar)))
  c.freeTemp(dest)

proc genNewSeq(c: PCtx; n: PNode) =
  let dest = c.genx(n.sons[1])
  c.gABx(n, opcNewSeq, dest, c.genType(n.sons[1].typ.skipTypes(abstractVar)))
  let tmp = c.genx(n.sons[2])
  c.gABx(n, opcNewSeq, tmp, 0)
  c.freeTemp(dest)
  c.freeTemp(tmp)

proc genUnaryABC(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  let tmp = c.genx(n.sons[1])
  if dest < 0: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp)
  c.freeTemp(tmp)

proc genBinaryABC(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  let
    tmp = c.genx(n.sons[1])
    tmp2 = c.genx(n.sons[2])
  if dest < 0: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp, tmp2)
  c.freeTemp(tmp)
  c.freeTemp(tmp2)

proc genVarargsABC(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  if dest < 0: dest = getTemp(c, n.typ)
  var x = c.getTempRange(n.len-1, slotTempStr)
  for i in 1..n.len-1: 
    var r: TRegister = x+i-1
    c.gen(n.sons[i], r)
  c.gABC(n, opc, dest, x, n.len-1)
  c.freeTempRange(x, n.len)

proc isInt8Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int8) and n.intVal <= high(int8)

proc isInt16Lit(n: PNode): bool =
  if n.kind in {nkCharLit..nkUInt64Lit}:
    result = n.intVal >= low(int16) and n.intVal <= high(int16)

proc genAddSubInt(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  if n.sons[2].isInt8Lit:
    let tmp = c.genx(n.sons[1])
    if dest < 0: dest = c.getTemp(n.typ)
    c.gABI(n, succ(opc), dest, tmp, n.sons[2].intVal)
    c.freeTemp(tmp)
  else:
    genBinaryABC(c, n, dest, opc)

proc unused(n: PNode; x: TDest) {.inline.} =
  if x >= 0: InternalError(n.info, "not unused")

proc genConv(c: PCtx; n, arg: PNode; dest: var TDest; opc=opcConv) =
  let tmp = c.genx(arg)
  let t = genType(c, n.typ)
  if dest < 0: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, tmp)
  c.gABx(n, opc, 0, t)
  c.freeTemp(tmp)

proc genMagic(c: PCtx; n: PNode; dest: var TDest) =
  let m = n.sons[0].sym.magic
  case m
  of mAnd: c.genAndOr(n, opcFJmp, dest)
  of mOr:  c.genAndOr(n, opcTJmp, dest)
  of mUnaryLt:
    let tmp = c.genx(n.sons[1])
    if dest < 0: dest = c.getTemp(n.typ)
    c.gABI(n, opcSubImmInt, dest, tmp, 1)
    c.freeTemp(tmp)
  of mPred, mSubI, mSubI64:
    c.genAddSubInt(n, dest, opcSubInt)
  of mSucc, mAddI, mAddI64:
    c.genAddSubInt(n, dest, opcAddInt)
  of mInc, mDec:
    unused(n, dest)
    var d = c.genx(n.sons[1]).TDest
    c.genAddSubInt(n, d, if m == mInc: opcAddInt else: opcSubInt)
    c.freeTemp(d.TRegister)
  of mOrd, mChr, mArrToSeq: c.gen(n.sons[1], dest)
  of mNew, mNewFinalize:
    unused(n, dest)
    c.genNew(n)
  of mNewSeq:
    unused(n, dest)
    c.genNewSeq(n)
  of mNewString:
    genUnaryABC(c, n, dest, opcNewStr)
  of mNewStringOfCap:
    # we ignore the 'cap' argument and translate it as 'newString(0)'.
    # eval n.sons[1] for possible side effects:
    var tmp = c.genx(n.sons[1])
    c.gABx(n, opcLdImmInt, tmp, 0)
    if dest < 0: dest = c.getTemp(n.typ)
    c.gABC(n, opcNewStr, dest, tmp)
    c.freeTemp(tmp)
  of mLengthOpenArray, mLengthArray, mLengthSeq:
    genUnaryABC(c, n, dest, opcLenSeq)
  of mLengthStr:
    genUnaryABC(c, n, dest, opcLenStr)
  of mIncl, mExcl:
    unused(n, dest)
    var d = c.genx(n.sons[1])
    var tmp = c.genx(n.sons[2])
    c.gABC(n, if m == mIncl: opcIncl else: opcExcl, d, tmp)
    c.freeTemp(d)
    c.freeTemp(tmp)
  of mCard: genUnaryABC(c, n, dest, opcCard)
  of mMulI, mMulI64: genBinaryABC(c, n, dest, opcMulInt)
  of mDivI, mDivI64: genBinaryABC(c, n, dest, opcDivInt)
  of mModI, mModI64: genBinaryABC(c, n, dest, opcModInt)
  of mAddF64: genBinaryABC(c, n, dest, opcAddFloat)
  of mSubF64: genBinaryABC(c, n, dest, opcSubFloat)
  of mMulF64: genBinaryABC(c, n, dest, opcMulFloat)
  of mDivF64: genBinaryABC(c, n, dest, opcDivFloat)
  of mShrI, mShrI64: genBinaryABC(c, n, dest, opcShrInt)
  of mShlI, mShlI64: genBinaryABC(c, n, dest, opcShlInt)
  of mBitandI, mBitandI64: genBinaryABC(c, n, dest, opcBitandInt)
  of mBitorI, mBitorI64: genBinaryABC(c, n, dest, opcBitorInt)
  of mBitxorI, mBitxorI64: genBinaryABC(c, n, dest, opcBitxorInt)
  of mAddU: genBinaryABC(c, n, dest, opcAddu)
  of mSubU: genBinaryABC(c, n, dest, opcSubu)
  of mMulU: genBinaryABC(c, n, dest, opcMulu)
  of mDivU: genBinaryABC(c, n, dest, opcDivu)
  of mModU: genBinaryABC(c, n, dest, opcModu)
  of mEqI, mEqI64, mEqB, mEqEnum, mEqCh:
    genBinaryABC(c, n, dest, opcEqInt)
  of mLeI, mLeI64, mLeEnum, mLeCh, mLeB:
    genBinaryABC(c, n, dest, opcLeInt)
  of mLtI, mLtI64, mLtEnum, mLtCh, mLtB:
    genBinaryABC(c, n, dest, opcLtInt)
  of mEqF64: genBinaryABC(c, n, dest, opcEqFloat)
  of mLeF64: genBinaryABC(c, n, dest, opcLeFloat)
  of mLtF64: genBinaryABC(c, n, dest, opcLtFloat)
  of mLePtr, mLeU, mLeU64: genBinaryABC(c, n, dest, opcLeu)
  of mLtPtr, mLtU, mLtU64: genBinaryABC(c, n, dest, opcLtu)
  of mEqProc, mEqRef, mEqUntracedRef: genBinaryABC(c, n, dest, opcEqRef)
  of mXor: genBinaryABC(c, n, dest, opcXor)
  of mNot: genUnaryABC(c, n, dest, opcNot)
  of mUnaryMinusI, mUnaryMinusI64: genUnaryABC(c, n, dest, opcUnaryMinusInt)
  of mUnaryMinusF64: genUnaryABC(c, n, dest, opcUnaryMinusFloat)
  of mUnaryPlusI, mUnaryPlusI64, mUnaryPlusF64: gen(c, n.sons[1], dest)
  of mBitnotI, mBitnotI64: genUnaryABC(c, n, dest, opcBitnotInt)
  of mZe8ToI, mZe8ToI64, mZe16ToI, mZe16ToI64, mZe32ToI64, mZeIToI64,
     mToU8, mToU16, mToU32, mToFloat, mToBiggestFloat, mToInt, 
     mToBiggestInt, mCharToStr, mBoolToStr, mIntToStr, mInt64ToStr, 
     mFloatToStr, mCStrToStr, mStrToStr, mEnumToStr:
    genConv(c, n, n.sons[1], dest)
  of mEqStr: genBinaryABC(c, n, dest, opcEqStr)
  of mLeStr: genBinaryABC(c, n, dest, opcLeStr)
  of mLtStr: genBinaryABC(c, n, dest, opcLtStr)
  of mEqSet: genBinaryABC(c, n, dest, opcEqSet)
  of mLeSet: genBinaryABC(c, n, dest, opcLeSet)
  of mLtSet: genBinaryABC(c, n, dest, opcLtSet)
  of mMulSet: genBinaryABC(c, n, dest, opcMulSet)
  of mPlusSet: genBinaryABC(c, n, dest, opcPlusSet)
  of mMinusSet: genBinaryABC(c, n, dest, opcMinusSet)
  of mSymDiffSet: genBinaryABC(c, n, dest, opcSymdiffSet)
  of mConStrStr: genVarargsABC(c, n, dest, opcConcatStr)
  of mInSet: genBinaryABC(c, n, dest, opcContainsSet)
  of mRepr: genUnaryABC(c, n, dest, opcRepr)
  of mExit:
    unused(n, dest)
    var tmp = c.genx(n.sons[1])
    c.gABC(n, opcQuit, tmp)
    c.freeTemp(tmp)
  of mSetLengthStr, mSetLengthSeq:
    unused(n, dest)
    var d = c.genx(n.sons[1])
    var tmp = c.genx(n.sons[2])
    c.gABC(n, if m == mSetLengthStr: opcSetLenStr else: opcSetLenSeq, d, tmp)
    c.freeTemp(tmp)
  of mSwap: 
    unused(n, dest)
    var d = c.genx(n.sons[1])
    var tmp = c.genx(n.sons[2])
    c.gABC(n, opcSwap, d, tmp)
    c.freeTemp(tmp)
  of mIsNil: genUnaryABC(c, n, dest, opcIsNil)
  of mCopyStr:
    if dest < 0: dest = c.getTemp(n.typ)
    var
      tmp1 = c.genx(n.sons[1])
      tmp2 = c.genx(n.sons[2])
      tmp3 = c.getTemp(n.sons[2].typ)
    c.gABC(n, opcLenStr, tmp3, tmp1)
    c.gABC(n, opcSubstr, dest, tmp1, tmp2)
    c.gABC(n, opcSubstr, tmp3)
    c.freeTemp(tmp1)
    c.freeTemp(tmp2)
    c.freeTemp(tmp3)
  of mCopyStrLast:
    if dest < 0: dest = c.getTemp(n.typ)
    var
      tmp1 = c.genx(n.sons[1])
      tmp2 = c.genx(n.sons[2])
      tmp3 = c.genx(n.sons[3])
    c.gABC(n, opcSubstr, dest, tmp1, tmp2)
    c.gABC(n, opcSubstr, tmp3)
    c.freeTemp(tmp1)
    c.freeTemp(tmp2)
    c.freeTemp(tmp3)
  of mReset:
    unused(n, dest)
    var d = c.genx(n.sons[1])
    c.gABC(n, opcReset, d)
  of mOf: 
    if dest < 0: dest = c.getTemp(n.typ)
    var tmp = c.genx(n.sons[1])
    c.gABC(n, opcOf, dest, tmp)
    c.gABx(n, opcOf, 0, c.genType(n.sons[2].typ.skipTypes(abstractPtrs)))
    c.freeTemp(tmp)
  of mSizeOf:
    GlobalError(n.info, errCannotInterpretNodeX, renderTree(n))
  of mHigh:
    if dest < 0: dest = c.getTemp(n.typ)
    let tmp = c.genx(n.sons[1])
    if n.sons[1].typ.skipTypes(abstractVar).kind == tyString:
      c.gABI(n, opcLenStr, dest, tmp, 1)
    else:
      c.gABI(n, opcLenSeq, dest, tmp, 1)
    c.freeTemp(tmp)
  of mEcho:
    unused(n, dest)
    for i in 1.. <n.len:
      var d = c.genx(n.sons[i])
      c.gABC(n, opcEcho, d)
      c.freeTemp(d)
  of mAppendStrCh: InternalError(n.info, "cannot generate code for: " & $m)
  of mAppendStrStr: InternalError(n.info, "cannot generate code for: " & $m)
  of mAppendSeqElem: InternalError(n.info, "cannot generate code for: " & $m)
  of mParseExprToAst: InternalError(n.info, "cannot generate code for: " & $m)
  of mParseStmtToAst: InternalError(n.info, "cannot generate code for: " & $m)
  of mExpandToAst: InternalError(n.info, "cannot generate code for: " & $m)
  of mTypeTrait: InternalError(n.info, "cannot generate code for: " & $m)
  of mIs: InternalError(n.info, "cannot generate code for: " & $m)
  of mSlurp: InternalError(n.info, "cannot generate code for: " & $m)
  of mStaticExec: InternalError(n.info, "cannot generate code for: " & $m)
  of mNLen: InternalError(n.info, "cannot generate code for: " & $m)
  of mNChild: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetChild: InternalError(n.info, "cannot generate code for: " & $m)
  of mNAdd: InternalError(n.info, "cannot generate code for: " & $m)
  of mNAddMultiple: InternalError(n.info, "cannot generate code for: " & $m)
  of mNDel: InternalError(n.info, "cannot generate code for: " & $m)
  of mNKind: InternalError(n.info, "cannot generate code for: " & $m)
  of mNIntVal: InternalError(n.info, "cannot generate code for: " & $m)
  of mNFloatVal: InternalError(n.info, "cannot generate code for: " & $m) 
  of mNSymbol: InternalError(n.info, "cannot generate code for: " & $m)
  of mNIdent: InternalError(n.info, "cannot generate code for: " & $m)
  of mNGetType: InternalError(n.info, "cannot generate code for: " & $m)
  of mNStrVal: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetIntVal: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetFloatVal: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetSymbol: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetIdent: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetType: InternalError(n.info, "cannot generate code for: " & $m)
  of mNSetStrVal: InternalError(n.info, "cannot generate code for: " & $m)
  of mNNewNimNode: InternalError(n.info, "cannot generate code for: " & $m)
  of mNCopyNimNode: InternalError(n.info, "cannot generate code for: " & $m)
  of mNCopyNimTree: InternalError(n.info, "cannot generate code for: " & $m)
  of mNBindSym: InternalError(n.info, "cannot generate code for: " & $m)
  of mStrToIdent: InternalError(n.info, "cannot generate code for: " & $m)
  of mIdentToStr: InternalError(n.info, "cannot generate code for: " & $m)
  of mEqIdent: InternalError(n.info, "cannot generate code for: " & $m)
  of mEqNimrodNode: InternalError(n.info, "cannot generate code for: " & $m)
  of mNLineInfo: InternalError(n.info, "cannot generate code for: " & $m)
  of mNHint: InternalError(n.info, "cannot generate code for: " & $m)
  of mNWarning: InternalError(n.info, "cannot generate code for: " & $m)
  of mNError: InternalError(n.info, "cannot generate code for: " & $m)
  of mNCallSite: InternalError(n.info, "cannot generate code for: " & $m)  
  else:
    # XXX get rid of these: mMinI, mMaxI, mMinI64, mMaxI64, mMinF64, mMaxF64
    # mGCref, mGCunref, mEqCString, mAbsI, mAbsI64, mAbsF64
    InternalError(n.info, "cannot generate code for: " & $m)

const
  atomicTypes = {tyBool, tyChar,
    tyExpr, tyStmt, tyTypeDesc,
    tyEnum,
    tyOrdinal,
    tyRange,
    tyProc,
    tyPointer, tyOpenArray,
    tyString, tyCString,
    tyInt, tyInt8, tyInt16, tyInt32, tyInt64,
    tyFloat, tyFloat32, tyFloat64, tyFloat128,
    tyUInt, tyUInt8, tyUInt16, tyUInt32, tyUInt64}

proc requiresCopy(n: PNode): bool =
  if n.typ.skipTypes(abstractInst).kind in atomicTypes:
    result = false
  elif n.kind in ({nkCurly, nkBracket, nkPar, nkObjConstr}+nkCallKinds):
    result = false
  else:
    result = true

proc unnecessaryIndirection(n: PNode): bool =
  n.typ.skipTypes(abstractInst).kind == tyRef

proc skipDeref(n: PNode): PNode =
  if n.kind in {nkDerefExpr, nkHiddenDeref} and unnecessaryIndirection(n):
    result = n.sons[0]
  else:
    result = n

proc genAddrDeref(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) = 
  # a nop for certain types
  if unnecessaryIndirection(n):
    gen(c, n.sons[0], dest)
  else:
    let tmp = c.genx(n.sons[0])
    if dest < 0: dest = c.getTemp(n.typ)
    gABC(c, n, opc, dest, tmp)
    c.freeTemp(tmp)

proc whichAsgnOpc(n: PNode): TOpcode =
  case n.typ.skipTypes(abstractRange).kind
  of tyBool, tyChar, tyEnum, tyOrdinal, tyInt..tyInt64, tyUInt..tyUInt64:
    opcAsgnInt
  of tyString, tyCString:
    opcAsgnStr
  of tyFloat..tyFloat128:
    opcAsgnFloat
  of tyRef, tyNil:
    opcAsgnRef
  else:
    opcAsgnComplex

proc isRef(t: PType): bool = t.skipTypes(abstractRange).kind == tyRef

proc whichAsgnOpc(n: PNode; opc: TOpcode): TOpcode =
  if isRef(n.typ): succ(opc) else: opc

proc genAsgn(c: PCtx; dest: TDest; ri: PNode; requiresCopy: bool) =
  let tmp = c.genx(ri)
  assert dest >= 0
  gABC(c, ri, whichAsgnOpc(ri), dest, tmp)
  c.freeTemp(tmp)

proc genAsgn(c: PCtx; le, ri: PNode; requiresCopy: bool) =
  case le.kind
  of nkBracketExpr:
    let dest = c.genx(le.sons[0])
    let idx = c.genx(le.sons[1])
    let tmp = c.genx(ri)
    if le.typ.skipTypes(abstractVarRange).kind in {tyString, tyCString}:
      c.gABC(le, opcWrStrIdx, dest, idx, tmp)
    else:
      c.gABC(le, whichAsgnOpc(le, opcWrArr), dest, idx, tmp)
    c.freeTemp(tmp)
  of nkDotExpr:
    let dest = c.genx(le.sons[0])
    let idx = c.genx(le.sons[1])
    let tmp = c.genx(ri)
    c.gABC(le, whichAsgnOpc(le, opcWrObj), dest, idx, tmp)
    c.freeTemp(tmp)
  of nkSym:
    let s = le.sym
    if sfGlobal in s.flags:
      withTemp(tmp, le.typ):
        gen(c, ri, tmp)
        c.gABx(le, whichAsgnOpc(le, opcWrGlobal), tmp, s.position)
    else:
      InternalAssert s.position > 0 or (s.position == 0 and
                                        s.kind in {skParam, skResult})
      var dest: TRegister = s.position + ord(s.kind == skParam)
      gen(c, ri, dest)
  else:
    let dest = c.genx(le)
    genAsgn(c, dest, ri, requiresCopy)

proc genLit(c: PCtx; n: PNode; dest: var TDest) =
  if dest < 0: dest = c.getTemp(n.typ)
  let lit = genLiteral(c, n)
  c.gABx(n, opcLdConst, dest, lit)

proc genRdVar(c: PCtx; n: PNode; dest: var TDest) =
  let s = n.sym
  if sfGlobal in s.flags:
    if dest < 0: dest = c.getTemp(s.typ)
    if s.position == 0:
      c.globals.add(s.ast)
      s.position = c.globals.len
      # XXX var g = codeHere() ?
    c.gABx(n, opcLdGlobal, dest, s.position)
  else:
    if s.position > 0 or (s.position == 0 and s.kind in {skParam, skResult}):
      if dest < 0:
        dest = s.position + ord(s.kind == skParam)
      else:
        # we need to generate an assignment:
        genAsgn(c, dest, n, c.prc.slots[dest].kind >= slotSomeTemp)
    else:
      InternalError(n.info, s.name.s & " " & $s.position)

proc genAccess(c: PCtx; n: PNode; dest: var TDest; opc: TOpcode) =
  let a = c.genx(n.sons[0])
  let b = c.genx(n.sons[1])
  if dest < 0: dest = c.getTemp(n.typ)
  c.gABC(n, opc, dest, a, b)
  c.freeTemp(a)
  c.freeTemp(b)

proc genObjAccess(c: PCtx; n: PNode; dest: var TDest) =
  genAccess(c, n, dest, opcLdObj)

proc genArrAccess(c: PCtx; n: PNode; dest: var TDest) =
  genAccess(c, n, dest, opcLdArr)

proc getNullValue*(typ: PType, info: TLineInfo): PNode
proc getNullValueAux(obj: PNode, result: PNode) = 
  case obj.kind
  of nkRecList:
    for i in countup(0, sonsLen(obj) - 1): getNullValueAux(obj.sons[i], result)
  of nkRecCase:
    getNullValueAux(obj.sons[0], result)
    for i in countup(1, sonsLen(obj) - 1): 
      getNullValueAux(lastSon(obj.sons[i]), result)
  of nkSym:
    addSon(result, getNullValue(obj.sym.typ, result.info))
  else: InternalError(result.info, "getNullValueAux")
  
proc getNullValue(typ: PType, info: TLineInfo): PNode = 
  var t = skipTypes(typ, abstractRange-{tyTypeDesc})
  result = emptyNode
  case t.kind
  of tyBool, tyEnum, tyChar, tyInt..tyInt64: 
    result = newNodeIT(nkIntLit, info, t)
  of tyUInt..tyUInt64:
    result = newNodeIT(nkUIntLit, info, t)
  of tyFloat..tyFloat128: 
    result = newNodeIt(nkFloatLit, info, t)
  of tyVar, tyPointer, tyPtr, tyCString, tySequence, tyString, tyExpr, 
     tyStmt, tyTypeDesc, tyProc, tyRef:
    result = newNodeIT(nkNilLit, info, t)
  of tyObject: 
    result = newNodeIT(nkPar, info, t)
    getNullValueAux(t.n, result)
    # initialize inherited fields:
    var base = t.sons[0]
    while base != nil:
      getNullValueAux(skipTypes(base, skipPtrs).n, result)
      base = base.sons[0]
  of tyArray, tyArrayConstr: 
    result = newNodeIT(nkBracket, info, t)
    for i in countup(0, int(lengthOrd(t)) - 1): 
      addSon(result, getNullValue(elemType(t), info))
  of tyTuple:
    result = newNodeIT(nkPar, info, t)
    for i in countup(0, sonsLen(t) - 1):
      addSon(result, getNullValue(t.sons[i], info))
  of tySet:
    result = newNodeIT(nkCurly, info, t)
  else: InternalError("getNullValue: " & $t.kind)

proc setSlot(c: PCtx; v: PSym) =
  # XXX generate type initialization here?
  if v.position == 0:
    v.position = c.prc.maxSlots
    c.prc.slots[v.position] = (inUse: true, kind: slotFixed)
    inc c.prc.maxSlots
    echo v.name.s, " has position ", v.position

proc genVarSection(c: PCtx; n: PNode) =
  for a in n:
    if a.kind == nkCommentStmt: continue
    #assert(a.sons[0].kind == nkSym) can happen for transformed vars
    if a.kind == nkVarTuple:
      let tmp = c.genx(a.lastSon)
      for i in 0 .. a.len-3:
        setSlot(c, a[i].sym)
        # v = t[i]
        var v: TDest = -1
        genRdVar(c, a[i], v)
        c.gABC(n, opcLdObj, v, tmp, i)
        # XXX globals?
      c.freeTemp(tmp)
    elif a.sons[0].kind == nkSym:
      let s = a.sons[0].sym
      if sfGlobal in s.flags:
        if s.position == 0:
          let sa = if s.ast.isNil: getNullValue(s.typ, a.info) else: s.ast
          c.globals.add(sa)
          s.position = c.globals.len
        if a.sons[2].kind == nkEmpty:
          when false:
            withTemp(tmp, s.typ):
              c.gABx(a, opcLdNull, tmp, c.genType(s.typ))
              c.gABx(a, whichAsgnOpc(a.sons[0], opcWrGlobal), tmp, s.position)
        else:
          let tmp = genx(c, a.sons[2])
          c.gABx(a, whichAsgnOpc(a.sons[0], opcWrGlobal), tmp, s.position)
          c.freeTemp(tmp)
      else:
        setSlot(c, s)
        if a.sons[2].kind == nkEmpty:
          c.gABx(a, opcLdNull, s.position, c.genType(s.typ))
        else:
          gen(c, a.sons[2], s.position.TRegister)
    else:
      # assign to a.sons[0]; happens for closures
      if a.sons[2].kind == nkEmpty:
        let tmp = genx(c, a.sons[0])
        c.gABx(a, opcLdNull, tmp, c.genType(a.sons[0].typ))
        c.freeTemp(tmp)
      else:
        genAsgn(c, a.sons[0], a.sons[2], true)

proc genProc*(c: PCtx; s: PSym): int

proc gen(c: PCtx; n: PNode; dest: var TDest) =
  case n.kind
  of nkSym:
    let s = n.sym
    case s.kind
    of skVar, skForVar, skTemp, skLet, skParam, skResult:
      genRdVar(c, n, dest)
    of skProc, skConverter, skMacro, skMethod, skIterator:
      genLit(c, n, dest)
    of skConst:
      gen(c, s.ast, dest)
    of skEnumField:
      if dest < 0: dest = c.getTemp(n.typ)
      if s.position >= low(int16) and s.position <= high(int16):
        c.gABx(n, opcLdImmInt, dest, s.position)
      else:
        var lit = genLiteral(c, newIntNode(nkIntLit, s.position))
        c.gABx(n, opcLdConst, dest, lit)
    of skField:
      InternalAssert dest < 0
      if s.position > high(dest):
        InternalError(n.info, 
          "too large offset! cannot generate code for: " & s.name.s)
      dest = s.position
    else:
      InternalError(n.info, "cannot generate code for: " & s.name.s)
  of nkCallKinds:
    if n.sons[0].kind == nkSym and n.sons[0].sym.magic != mNone:
      genMagic(c, n, dest)
    else:
      genCall(c, n, dest)
  of nkCharLit..nkInt64Lit:
    if isInt16Lit(n):
      if dest < 0: dest = c.getTemp(n.typ)
      c.gABx(n, opcLdImmInt, dest, n.intVal.int)
    else:
      genLit(c, n, dest)
  of nkUIntLit..nkNilLit: genLit(c, n, dest)
  of nkAsgn, nkFastAsgn: 
    unused(n, dest)
    genAsgn(c, n.sons[0], n.sons[1], n.kind == nkAsgn)
  of nkDotExpr: genObjAccess(c, n, dest)
  of nkBracketExpr: genArrAccess(c, n, dest)
  of nkDerefExpr, nkHiddenDeref: genAddrDeref(c, n, dest, opcDeref)
  of nkAddr, nkHiddenAddr: genAddrDeref(c, n, dest, opcAddr)
  of nkWhenStmt, nkIfStmt, nkIfExpr: genIf(c, n, dest)
  of nkCaseStmt: genCase(c, n, dest)
  of nkWhileStmt:
    unused(n, dest)
    genWhile(c, n)
  of nkBlockExpr, nkBlockStmt: genBlock(c, n, dest)
  of nkReturnStmt:
    unused(n, dest)
    genReturn(c, n)
  of nkRaiseStmt:
    unused(n, dest)
    genRaise(c, n)
  of nkBreakStmt:
    unused(n, dest)
    genBreak(c, n)
  of nkTryStmt: genTry(c, n, dest)
  of nkStmtList:
    unused(n, dest)
    for x in n: gen(c, x)
  of nkStmtListExpr:
    let L = n.len-1
    for i in 0 .. <L: gen(c, n.sons[i])
    gen(c, n.sons[L], dest)
  of nkDiscardStmt:
    unused(n, dest)
    gen(c, n.sons[0])
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    genConv(c, n, n.sons[1], dest)
  of nkVarSection, nkLetSection:
    unused(n, dest)
    genVarSection(c, n)
  of declarativeDefs:
    unused(n, dest)
  of nkLambdaKinds:
    let s = n.sons[namePos].sym
    discard genProc(c, s)
    genLit(c, n.sons[namePos], dest)
  of nkEmpty, nkCommentStmt, nkTypeSection, nkConstSection, nkPragma,
     nkTemplateDef, nkIncludeStmt, nkImportStmt:
    unused(n, dest)
  else:
    #of nkCurly, nkBracket, nkPar:
    InternalError n.info, "too implement " & $n.kind

proc genStmt*(c: PCtx; n: PNode): int =
  result = c.code.len
  var d: TDest = -1
  gen(c, n, d)
  let last = c.code.len-1
  if last >= 0 and c.code[last].opcode == opcEof:
    # since we can re-use the EOF nothing happened:
    result = last
  else:
    gABC(c, n, opcEof)
  InternalAssert d < 0

proc genParams(c: PCtx; params: PNode) =
  # res.sym.position is already 0
  c.prc.slots[0] = (inUse: true, kind: slotFixed)
  for i in 1.. <params.len:
    let param = params.sons[i].sym
    c.prc.slots[i] = (inUse: true, kind: slotFixed)
  c.prc.maxSlots = max(params.len, 1)

proc genProc(c: PCtx; s: PSym): int =
  let x = s.ast.sons[optimizedCodePos]
  if x.kind == nkEmpty:
    result = c.code.len+1 # skip the jump instruction
    s.ast.sons[optimizedCodePos] = newIntNode(nkIntLit, result)
    # thanks to the jmp we can add top level statements easily and also nest
    # procs easily:
    let body = s.getBody
    let procStart = c.xjmp(body, opcJmp, 0)
    var p = PProc(blocks: @[])
    let oldPrc = c.prc
    c.prc = p
    # iterate over the parameters and allocate space for them:
    genParams(c, s.typ.n)
    gen(c, body)
    # generate final 'return' statement:
    c.gABC(body, opcRet)
    c.patch(procStart)
    s.position = c.prc.maxSlots
    c.prc = oldPrc
  else:
    result = x.intVal.int
