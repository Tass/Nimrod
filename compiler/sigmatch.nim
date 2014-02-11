#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the signature matching for resolving
## the call to overloaded procs, generic procs and operators.

import 
  intsets, ast, astalgo, semdata, types, msgs, renderer, lookups, semtypinst,
  magicsys, condsyms, idents, lexer, options, parampatterns, strutils

when not defined(noDocgen):
  import docgen

type
  TCandidateState* = enum 
    csEmpty, csMatch, csNoMatch

  TCandidate* {.final.} = object
    c*: PContext
    exactMatches*: int       # also misused to prefer iters over procs
    genericMatches: int      # also misused to prefer constraints
    subtypeMatches: int
    intConvMatches: int      # conversions to int are not as expensive
    convMatches: int
    state*: TCandidateState
    callee*: PType           # may not be nil!
    calleeSym*: PSym         # may be nil
    calleeScope*: int        # scope depth:
                             # is this a top-level symbol or a nested proc?
    call*: PNode             # modified call
    bindings*: TIdTable      # maps types to types
    baseTypeMatch: bool      # needed for conversions from T to openarray[T]
                             # for example
    proxyMatch*: bool        # to prevent instantiations
    genericConverter*: bool  # true if a generic converter needs to
                             # be instantiated
    typedescMatched: bool
    inheritancePenalty: int  # to prefer closest father object type
    errors*: seq[string]     # additional clarifications to be displayed to the
                             # user if overload resolution fails

  TTypeRelation* = enum      # order is important!
    isNone, isConvertible,
    isIntConv,
    isSubtype,
    isSubrange,              # subrange of the wanted type; no type conversion
                             # but apart from that counts as ``isSubtype``
    isInferred,              # generic proc was matched against a concrete type
    isInferredConvertible,   # same as above, but requiring proc CC conversion
    isGeneric,
    isFromIntLit,            # conversion *from* int literal; proven safe
    isEqual
  
const
  isNilConversion = isConvertible # maybe 'isIntConv' fits better?
    
proc markUsed*(n: PNode, s: PSym)

proc initCandidateAux(ctx: PContext,
                      c: var TCandidate, callee: PType) {.inline.} =
  c.c = ctx
  c.exactMatches = 0
  c.subtypeMatches = 0
  c.convMatches = 0
  c.intConvMatches = 0
  c.genericMatches = 0
  c.state = csEmpty
  c.callee = callee
  c.call = nil
  c.baseTypeMatch = false
  c.genericConverter = false
  c.inheritancePenalty = 0

proc initCandidate*(ctx: PContext, c: var TCandidate, callee: PType) =
  initCandidateAux(ctx, c, callee)
  c.calleeSym = nil
  initIdTable(c.bindings)

proc put(t: var TIdTable, key, val: PType) {.inline.} =
  idTablePut(t, key, val)

proc initCandidate*(ctx: PContext, c: var TCandidate, callee: PSym,
                    binding: PNode, calleeScope = -1) =
  initCandidateAux(ctx, c, callee.typ)
  c.calleeSym = callee
  if callee.kind in skProcKinds and calleeScope == -1:
    if callee.originatingModule == ctx.module:
      let rootSym = if sfFromGeneric notin callee.flags: callee
                    else: callee.owner
      c.calleeScope = rootSym.scope.depthLevel
    else:
      c.calleeScope = 1
  else:
    c.calleeScope = calleeScope
  initIdTable(c.bindings)
  c.errors = nil
  if binding != nil and callee.kind in routineKinds:
    var typeParams = callee.ast[genericParamsPos]
    for i in 1..min(sonsLen(typeParams), sonsLen(binding)-1):
      var formalTypeParam = typeParams.sons[i-1].typ
      var bound = binding[i].typ
      if bound != nil and formalTypeParam.kind != tyTypeDesc:
        bound = bound.skipTypes({tyTypeDesc})
      assert bound != nil
      put(c.bindings, formalTypeParam, bound)

proc newCandidate*(ctx: PContext, callee: PSym,
                   binding: PNode, calleeScope = -1): TCandidate =
  initCandidate(ctx, result, callee, binding, calleeScope)

proc copyCandidate(a: var TCandidate, b: TCandidate) = 
  a.c = b.c
  a.exactMatches = b.exactMatches
  a.subtypeMatches = b.subtypeMatches
  a.convMatches = b.convMatches
  a.intConvMatches = b.intConvMatches
  a.genericMatches = b.genericMatches
  a.state = b.state
  a.callee = b.callee
  a.calleeSym = b.calleeSym
  a.call = copyTree(b.call)
  a.baseTypeMatch = b.baseTypeMatch
  copyIdTable(a.bindings, b.bindings)

proc sumGeneric(t: PType): int =
  var t = t
  while true:
    case t.kind
    of tyGenericInst, tyArray, tyRef, tyPtr, tyDistinct, tyArrayConstr,
        tyOpenArray, tyVarargs, tySet, tyRange, tySequence, tyGenericBody:
      t = t.lastSon
      inc result
    of tyVar:
      # but do not make 'var T' more specific than 'T'!
      t = t.sons[0]
    of tyGenericInvokation, tyTuple:
      result = ord(t.kind == tyGenericInvokation)
      for i in 0 .. <t.len: result += t.sons[i].sumGeneric
      break
    of tyGenericParam, tyExpr, tyStatic, tyStmt, tyTypeDesc: break
    else: return 0

proc complexDisambiguation(a, b: PType): int =
  var x, y: int
  for i in 1 .. <a.len: x += a.sons[i].sumGeneric
  for i in 1 .. <b.len: y += b.sons[i].sumGeneric
  result = x - y
  when false:
    proc betterThan(a, b: PType): bool {.inline.} = a.sumGeneric > b.sumGeneric

    if a.len > 1 and b.len > 1:
      let aa = a.sons[1].sumGeneric
      let bb = b.sons[1].sumGeneric
      var a = a
      var b = b
      
      if aa < bb: swap(a, b)
      # all must be better
      for i in 2 .. <min(a.len, b.len):
        if not a.sons[i].betterThan(b.sons[i]): return 0
      # a must be longer or of the same length as b:
      result = a.len - b.len

proc cmpCandidates*(a, b: TCandidate): int =
  result = a.exactMatches - b.exactMatches
  if result != 0: return
  result = a.genericMatches - b.genericMatches
  if result != 0: return
  result = a.subtypeMatches - b.subtypeMatches
  if result != 0: return
  result = a.intConvMatches - b.intConvMatches
  if result != 0: return
  result = a.convMatches - b.convMatches
  if result != 0: return
  result = a.calleeScope - b.calleeScope
  if result != 0: return
  # the other way round because of other semantics:
  result = b.inheritancePenalty - a.inheritancePenalty
  if result != 0: return
  # prefer more specialized generic over more general generic:
  result = complexDisambiguation(a.callee, b.callee)

proc writeMatches*(c: TCandidate) = 
  writeln(stdout, "exact matches: " & $c.exactMatches)
  writeln(stdout, "subtype matches: " & $c.subtypeMatches)
  writeln(stdout, "conv matches: " & $c.convMatches)
  writeln(stdout, "intconv matches: " & $c.intConvMatches)
  writeln(stdout, "generic matches: " & $c.genericMatches)

proc argTypeToString(arg: PNode): string =
  if arg.kind in nkSymChoices:
    result = typeToString(arg[0].typ)
    for i in 1 .. <arg.len:
      result.add(" | ")
      result.add typeToString(arg[i].typ)
  else:
    result = arg.typ.typeToString

proc describeArgs*(c: PContext, n: PNode, startIdx = 1): string =
  result = ""
  for i in countup(startIdx, n.len - 1):
    var arg = n.sons[i]
    if n.sons[i].kind == nkExprEqExpr: 
      add(result, renderTree(n.sons[i].sons[0]))
      add(result, ": ")
      if arg.typ.isNil:
        arg = c.semOperand(c, n.sons[i].sons[1])
        n.sons[i].typ = arg.typ
        n.sons[i].sons[1] = arg
    else:
      if arg.typ.isNil:
        arg = c.semOperand(c, n.sons[i])
        n.sons[i] = arg
    if arg.typ.kind == tyError: return
    add(result, argTypeToString(arg))
    if i != sonsLen(n) - 1: add(result, ", ")

proc typeRel*(c: var TCandidate, f, aOrig: PType, doBind = true): TTypeRelation
proc concreteType(c: TCandidate, t: PType): PType = 
  case t.kind
  of tyArrayConstr: 
    # make it an array
    result = newType(tyArray, t.owner)
    addSonSkipIntLit(result, t.sons[0]) # XXX: t.owner is wrong for ID!
    addSonSkipIntLit(result, t.sons[1]) # XXX: semantic checking for the type?
  of tyNil:
    result = nil              # what should it be?
  of tyGenericParam, tyAnything:
    result = t
    while true: 
      result = PType(idTableGet(c.bindings, t))
      if result == nil:
        break # it's ok, no match
        # example code that triggers it:
        # proc sort[T](cmp: proc(a, b: T): int = cmp)
      if result.kind != tyGenericParam: break
  of tyGenericInvokation:
    internalError("cannot resolve type: " & typeToString(t))
    result = t
  else:
    result = t                # Note: empty is valid here
  
proc handleRange(f, a: PType, min, max: TTypeKind): TTypeRelation = 
  if a.kind == f.kind: 
    result = isEqual
  else:
    let ab = skipTypes(a, {tyRange})
    let k = ab.kind
    if k == f.kind: result = isSubrange
    elif k == tyInt and f.kind in {tyRange, tyInt8..tyInt64, 
                                   tyUInt..tyUInt64} and
        isIntLit(ab) and ab.n.intVal >= firstOrd(f) and
                         ab.n.intVal <= lastOrd(f):
      # integer literal in the proper range; we want ``i16 + 4`` to stay an
      # ``int16`` operation so we declare the ``4`` pseudo-equal to int16
      result = isFromIntLit
    elif f.kind == tyInt and k in {tyInt8..tyInt32}:
      result = isIntConv
    elif k >= min and k <= max: 
      result = isConvertible
    elif a.kind == tyRange and a.sons[0].kind in {tyInt..tyInt64, 
                                                  tyUInt8..tyUInt32} and
                         a.n[0].intVal >= firstOrd(f) and
                         a.n[1].intVal <= lastOrd(f):
      result = isConvertible
    else: result = isNone
    #elif f.kind == tyInt and k in {tyInt..tyInt32}: result = isIntConv
    #elif f.kind == tyUInt and k in {tyUInt..tyUInt32}: result = isIntConv

proc isConvertibleToRange(f, a: PType): bool =
  # be less picky for tyRange, as that it is used for array indexing:
  if f.kind in {tyInt..tyInt64, tyUInt..tyUInt64} and
     a.kind in {tyInt..tyInt64, tyUInt..tyUInt64}:
    result = true
  elif f.kind in {tyFloat..tyFloat128} and
       a.kind in {tyFloat..tyFloat128}:
    result = true

proc handleFloatRange(f, a: PType): TTypeRelation =
  if a.kind == f.kind:
    result = isEqual
  else:
    let ab = skipTypes(a, {tyRange})
    var k = ab.kind
    if k == f.kind: result = isSubrange
    elif isFloatLit(ab): result = isFromIntLit
    elif isIntLit(ab): result = isConvertible
    elif k >= tyFloat and k <= tyFloat128:
      # conversion to "float32" is not as good:
      if f.kind == tyFloat32: result = isConvertible
      else: result = isIntConv
    else: result = isNone
  
proc isObjectSubtype(a, f: PType): int =
  var t = a
  assert t.kind == tyObject
  var depth = 0
  while t != nil and not sameObjectTypes(f, t): 
    assert t.kind == tyObject
    t = t.sons[0]
    if t == nil: break
    t = skipTypes(t, {tyGenericInst})
    inc depth
  if t != nil:
    result = depth

proc minRel(a, b: TTypeRelation): TTypeRelation = 
  if a <= b: result = a
  else: result = b
  
proc recordRel(c: var TCandidate, f, a: PType): TTypeRelation =
  result = isNone
  if sameType(f, a): result = isEqual
  elif sonsLen(a) == sonsLen(f):
    result = isEqual
    let firstField = if f.kind == tyTuple: 0
                     else: 1 
    for i in countup(firstField, sonsLen(f) - 1):
      var m = typeRel(c, f.sons[i], a.sons[i])
      if m < isSubtype: return isNone
      result = minRel(result, m)
    if f.n != nil and a.n != nil:
      for i in countup(0, sonsLen(f.n) - 1):
        # check field names:
        if f.n.sons[i].kind != nkSym: internalError(f.n.info, "recordRel")
        elif a.n.sons[i].kind != nkSym: internalError(a.n.info, "recordRel")
        else:
          var x = f.n.sons[i].sym
          var y = a.n.sons[i].sym
          if f.kind == tyObject and typeRel(c, x.typ, y.typ) < isSubtype:
            return isNone
          if x.name.id != y.name.id: return isNone

proc allowsNil(f: PType): TTypeRelation {.inline.} =
  result = if tfNotNil notin f.flags: isSubtype else: isNone

proc inconsistentVarTypes(f, a: PType): bool {.inline.} =
  result = f.kind != a.kind and (f.kind == tyVar or a.kind == tyVar)

proc procParamTypeRel(c: var TCandidate, f, a: PType,
                      result: var TTypeRelation) =
  var
    m: TTypeRelation
    f = f
    
  if a.isMetaType:
    if f.isMetaType:
      # we are matching a generic proc (as proc param)
      # to another generic type appearing in the proc
      # sigunature. there is a change that the target
      # type is already fully-determined, so we are 
      # going to try resolve it
      f = generateTypeInstance(c.c, c.bindings, c.call.info, f)
      if f == nil or f.isMetaType:
        # no luck resolving the type, so the inference fails
        result = isNone
        return
    let reverseRel = typeRel(c, a, f)
    if reverseRel == isGeneric:
      m = isInferred
  else:
    m = typeRel(c, f, a)

  if m <= isSubtype or inconsistentVarTypes(f, a):
    result = isNone
    return
  else:
    result = minRel(m, result)

proc procTypeRel(c: var TCandidate, f, a: PType): TTypeRelation =
  case a.kind
  of tyProc:
    if sonsLen(f) != sonsLen(a): return
    # Note: We have to do unification for the parameters before the
    # return type!
    result = isEqual      # start with maximum; also correct for no
                          # params at all
    for i in countup(1, sonsLen(f)-1):
      procParamTypeRel(c, f.sons[i], a.sons[i], result)
    if f.sons[0] != nil:
      if a.sons[0] != nil:
        procParamTypeRel(c, f.sons[0], a.sons[0], result)
      else:
        return isNone
    elif a.sons[0] != nil:
      return isNone
    if tfNoSideEffect in f.flags and tfNoSideEffect notin a.flags:
      return isNone
    elif tfThread in f.flags and a.flags * {tfThread, tfNoSideEffect} == {}:
      # noSideEffect implies ``tfThread``! XXX really?
      return isNone
    elif f.flags * {tfIterator} != a.flags * {tfIterator}:
      return isNone
    elif f.callConv != a.callConv:
      # valid to pass a 'nimcall' thingie to 'closure':
      if f.callConv == ccClosure and a.callConv == ccDefault:
        result = if result != isInferred: isConvertible
                 else: isInferredConvertible
      else:
        return isNone
    when useEffectSystem:
      if not compatibleEffects(f, a): return isNone
  of tyNil: result = f.allowsNil
  else: discard

proc typeRangeRel(f, a: PType): TTypeRelation {.noinline.} =
  let
    a0 = firstOrd(a)
    a1 = lastOrd(a)
    f0 = firstOrd(f)
    f1 = lastOrd(f)
  if a0 == f0 and a1 == f1:
    result = isEqual
  elif a0 >= f0 and a1 <= f1:
    result = isConvertible
  elif a0 <= f1 and f0 <= a1:
    # X..Y and C..D overlap iff (X <= D and C <= Y)
    result = isConvertible
  else:
    result = isNone

proc matchUserTypeClass*(c: PContext, m: var TCandidate,
                         ff, a: PType): TTypeRelation =
  var body = ff.skipTypes({tyUserTypeClassInst})

  openScope(c)
  inc c.inTypeClass

  finally:
    dec c.inTypeClass
    closeScope(c)

  if ff.kind == tyUserTypeClassInst:
    for i in 1 .. <(ff.len - 1):
      var
        typeParamName = ff.base.sons[i-1].sym.name
        typ = ff.sons[i]
        param = newSym(skType, typeParamName, body.sym, body.sym.info)
        
      param.typ = makeTypeDesc(c, typ)
      addDecl(c, param)

  for param in body.n[0]:
    var
      dummyName: PNode
      dummyType: PType
    
    if param.kind == nkVarTy:
      dummyName = param[0]
      dummyType = makeVarType(c, a)
    else:
      dummyName = param
      dummyType = a

    internalAssert dummyName.kind == nkIdent
    var dummyParam = newSym(skType, dummyName.ident, body.sym, body.sym.info)
    dummyParam.typ = dummyType
    addDecl(c, dummyParam)

  var checkedBody = c.semTryExpr(c, copyTree(body.n[3]), bufferErrors = false)
  m.errors = bufferedMsgs
  clearBufferedMsgs()
  if checkedBody == nil: return isNone

  if checkedBody.kind == nkStmtList:
    for stmt in checkedBody:
      case stmt.kind
      of nkReturnStmt: discard
      of nkTypeSection: discard
      of nkConstDef: discard
      else: discard
    
  return isGeneric

proc typeRel(c: var TCandidate, f, aOrig: PType, doBind = true): TTypeRelation =
  # typeRel can be used to establish various relationships between types:
  #
  # 1) When used with concrete types, it will check for type equivalence
  # or a subtype relationship. 
  #
  # 2) When used with a concrete type against a type class (such as generic
  # signature of a proc), it will check whether the concrete type is a member
  # of the designated type class.
  #
  # 3) When used with two type classes, it will check whether the types
  # matching the first type class are a strict subset of the types matching
  # the other. This allows us to compare the signatures of generic procs in
  # order to give preferrence to the most specific one:
  #
  # seq[seq[any]] is a strict subset of seq[any] and hence more specific.

  result = isNone
  assert(f != nil)
  
  if f.kind == tyExpr:
    put(c.bindings, f, aOrig)
    return isGeneric

  assert(aOrig != nil)

  # var and static arguments match regular modifier-free types
  let a = aOrig.skipTypes({tyStatic, tyVar})
  
  if a.kind == tyGenericInst and
      skipTypes(f, {tyVar}).kind notin {
        tyGenericBody, tyGenericInvokation,
        tyGenericInst, tyGenericParam} + tyTypeClasses:
    return typeRel(c, f, lastSon(a))

  template bindingRet(res) =
    when res == isGeneric:
      let bound = aOrig.skipTypes({tyRange}).skipIntLit
      put(c.bindings, f, bound)
    return res

  template considerPreviousT(body: stmt) {.immediate.} =
    var prev = PType(idTableGet(c.bindings, f))
    if prev == nil: body
    else: return typeRel(c, prev, a)

  case a.kind
  of tyOr:
    # seq[int|string] vs seq[number]
    # both int and string must match against number
    for branch in a.sons:
      if typeRel(c, f, branch, false) == isNone:
        return isNone

    return isGeneric

  of tyAnd:
    # seq[Sortable and Iterable] vs seq[Sortable]
    # only one match is enough
    for branch in a.sons:
      if typeRel(c, f, branch, false) != isNone:
        return isGeneric

    return isNone

  of tyNot:
    case f.kind
    of tyNot:
      # seq[!int] vs seq[!number]
      # seq[float] matches the first, but not the second
      # we must turn the problem around:
      # is number a subset of int? 
      return typeRel(c, a.lastSon, f.lastSon)
 
    else:
      # negative type classes are essentially infinite,
      # so only the `any` type class is their superset
      return if f.kind == tyAnything: isGeneric
             else: isNone

  of tyAnything:
    return if f.kind == tyAnything: isGeneric
           else: isNone
  else: discard

  case f.kind
  of tyEnum:
    if a.kind == f.kind and sameEnumTypes(f, a): result = isEqual
    elif sameEnumTypes(f, skipTypes(a, {tyRange})): result = isSubtype
  of tyBool, tyChar:
    if a.kind == f.kind: result = isEqual
    elif skipTypes(a, {tyRange}).kind == f.kind: result = isSubtype
  of tyRange:
    if a.kind == f.kind:
      result = typeRel(c, base(f), base(a))
      # bugfix: accept integer conversions here
      #if result < isGeneric: result = isNone
      if result notin {isNone, isGeneric}:
        result = typeRangeRel(f, a)
    else:
      if skipTypes(f, {tyRange}).kind == a.kind:
        result = isIntConv
      elif isConvertibleToRange(skipTypes(f, {tyRange}), a):
        result = isConvertible  # a convertible to f
  of tyInt:      result = handleRange(f, a, tyInt8, tyInt32)
  of tyInt8:     result = handleRange(f, a, tyInt8, tyInt8)
  of tyInt16:    result = handleRange(f, a, tyInt8, tyInt16)
  of tyInt32:    result = handleRange(f, a, tyInt8, tyInt32)
  of tyInt64:    result = handleRange(f, a, tyInt, tyInt64)
  of tyUInt:     result = handleRange(f, a, tyUInt8, tyUInt32)
  of tyUInt8:    result = handleRange(f, a, tyUInt8, tyUInt8)
  of tyUInt16:   result = handleRange(f, a, tyUInt8, tyUInt16)
  of tyUInt32:   result = handleRange(f, a, tyUInt8, tyUInt32)
  of tyUInt64:   result = handleRange(f, a, tyUInt, tyUInt64)
  of tyFloat:    result = handleFloatRange(f, a)
  of tyFloat32:  result = handleFloatRange(f, a)
  of tyFloat64:  result = handleFloatRange(f, a)
  of tyFloat128: result = handleFloatRange(f, a)
  of tyVar:
    if aOrig.kind == tyVar: result = typeRel(c, f.base, aOrig.base)
    else: result = typeRel(c, f.base, aOrig)
  of tyArray, tyArrayConstr:
    # tyArrayConstr cannot happen really, but
    # we wanna be safe here
    case a.kind
    of tyArray, tyArrayConstr:
      var fRange = f.sons[0]
      if fRange.kind == tyGenericParam:
        var prev = PType(idTableGet(c.bindings, fRange))
        if prev == nil:
          put(c.bindings, fRange, a.sons[0])
          fRange = a
        else:
          fRange = prev
      result = typeRel(c, f.sons[1], a.sons[1])
      if result < isGeneric: result = isNone
      elif lengthOrd(fRange) != lengthOrd(a): result = isNone
    else: discard
  of tyOpenArray, tyVarargs:
    case a.kind
    of tyOpenArray, tyVarargs:
      result = typeRel(c, base(f), base(a))
      if result < isGeneric: result = isNone
    of tyArrayConstr: 
      if (f.sons[0].kind != tyGenericParam) and (a.sons[1].kind == tyEmpty): 
        result = isSubtype    # [] is allowed here
      elif typeRel(c, base(f), a.sons[1]) >= isGeneric: 
        result = isSubtype
    of tyArray: 
      if (f.sons[0].kind != tyGenericParam) and (a.sons[1].kind == tyEmpty): 
        result = isSubtype
      elif typeRel(c, base(f), a.sons[1]) >= isGeneric: 
        result = isConvertible
    of tySequence: 
      if (f.sons[0].kind != tyGenericParam) and (a.sons[0].kind == tyEmpty): 
        result = isConvertible
      elif typeRel(c, base(f), a.sons[0]) >= isGeneric: 
        result = isConvertible
    else: discard
  of tySequence:
    case a.kind
    of tySequence:
      if (f.sons[0].kind != tyGenericParam) and (a.sons[0].kind == tyEmpty):
        result = isSubtype
      else:
        result = typeRel(c, f.sons[0], a.sons[0])
        if result < isGeneric: result = isNone
        elif tfNotNil in f.flags and tfNotNil notin a.flags:
          result = isNilConversion
    of tyNil: result = f.allowsNil
    else: discard
  of tyOrdinal:
    if isOrdinalType(a):
      var x = if a.kind == tyOrdinal: a.sons[0] else: a
      if f.sons[0].kind == tyNone:
        result = isGeneric
      else:
        result = typeRel(c, f.sons[0], x)
        if result < isGeneric: result = isNone
    elif a.kind == tyGenericParam:
      result = isGeneric
  of tyForward: internalError("forward type in typeRel()")
  of tyNil:
    if a.kind == f.kind: result = isEqual
  of tyTuple: 
    if a.kind == tyTuple: result = recordRel(c, f, a)
  of tyObject:
    if a.kind == tyObject:
      if sameObjectTypes(f, a):
        result = isEqual
        # elif tfHasMeta in f.flags: result = recordRel(c, f, a)
      else:
        var depth = isObjectSubtype(a, f)
        if depth > 0:
          inc(c.inheritancePenalty, depth)
          result = isSubtype
  of tyDistinct:
    if (a.kind == tyDistinct) and sameDistinctTypes(f, a): result = isEqual
  of tySet: 
    if a.kind == tySet: 
      if (f.sons[0].kind != tyGenericParam) and (a.sons[0].kind == tyEmpty): 
        result = isSubtype
      else: 
        result = typeRel(c, f.sons[0], a.sons[0])
        if result <= isConvertible: 
          result = isNone     # BUGFIX!
  of tyPtr: 
    case a.kind
    of tyPtr: 
      result = typeRel(c, base(f), base(a))
      if result <= isConvertible: result = isNone
      elif tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
    of tyNil: result = f.allowsNil
    else: discard
  of tyRef: 
    case a.kind
    of tyRef:
      result = typeRel(c, base(f), base(a))
      if result <= isConvertible: result = isNone
      elif tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
    of tyNil: result = f.allowsNil
    else: discard
  of tyProc:
    result = procTypeRel(c, f, a)
    if result != isNone and tfNotNil in f.flags and tfNotNil notin a.flags:
      result = isNilConversion
  of tyPointer:
    case a.kind
    of tyPointer:
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil: result = f.allowsNil
    of tyProc:
      if a.callConv != ccClosure: result = isConvertible
    of tyPtr, tyCString: result = isConvertible
    else: discard
  of tyString: 
    case a.kind
    of tyString: 
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil: result = f.allowsNil
    else: discard
  of tyCString:
    # conversion from string to cstring is automatic:
    case a.kind
    of tyCString:
      if tfNotNil in f.flags and tfNotNil notin a.flags:
        result = isNilConversion
      else:
        result = isEqual
    of tyNil: result = f.allowsNil
    of tyString: result = isConvertible
    of tyPtr:
      if a.sons[0].kind == tyChar: result = isConvertible
    of tyArray: 
      if (firstOrd(a.sons[0]) == 0) and
          (skipTypes(a.sons[0], {tyRange}).kind in {tyInt..tyInt64}) and
          (a.sons[1].kind == tyChar): 
        result = isConvertible
    else: discard

  of tyEmpty:
    if a.kind == tyEmpty: result = isEqual

  of tyGenericInst:
    let roota = a.skipGenericAlias
    let rootf = f.skipGenericAlias
    if a.kind == tyGenericInst and roota.base == rootf.base:
      for i in 1 .. rootf.sonsLen-2:
        let ff = rootf.sons[i]
        let aa = roota.sons[i]
        result = typeRel(c, ff, aa)
        if result == isNone: return        
        if ff.kind == tyRange and result != isEqual: return isNone

      result = isGeneric
    else:
      result = typeRel(c, lastSon(f), a)

  of tyGenericBody:
    considerPreviousT:
      if a.kind == tyGenericInst and a.sons[0] == f:
        bindingRet isGeneric
      let ff = lastSon(f)
      if ff != nil: result = typeRel(c, ff, a)

  of tyGenericInvokation:
    var x = a.skipGenericAlias
    if x.kind == tyGenericInvokation or f.sons[0].kind != tyGenericBody:
      #InternalError("typeRel: tyGenericInvokation -> tyGenericInvokation")
      # simply no match for now:
      discard
    elif x.kind == tyGenericInst and 
          (f.sons[0] == x.sons[0]) and
          (sonsLen(x) - 1 == sonsLen(f)):
      for i in countup(1, sonsLen(f) - 1):
        if x.sons[i].kind == tyGenericParam:
          internalError("wrong instantiated type!")
        elif typeRel(c, f.sons[i], x.sons[i]) <= isSubtype: return 
      result = isGeneric
    else:
      result = typeRel(c, f.sons[0], x)
      if result != isNone:
        # we steal the generic parameters from the tyGenericBody:
        for i in countup(1, sonsLen(f) - 1):
          var x = PType(idTableGet(c.bindings, f.sons[0].sons[i - 1]))
          if x == nil or x.kind in {tyGenericInvokation, tyGenericParam}:
            internalError("wrong instantiated type!")
          put(c.bindings, f.sons[i], x)
  
  of tyAnd:
    considerPreviousT:
      for branch in f.sons:
        if typeRel(c, branch, aOrig) == isNone:
          return isNone

      bindingRet isGeneric

  of tyOr:
    considerPreviousT:
      for branch in f.sons:
        if typeRel(c, branch, aOrig) != isNone:
          bindingRet isGeneric
       
      return isNone

  of tyNot:
    considerPreviousT:
      for branch in f.sons:
        if typeRel(c, branch, aOrig) != isNone:
          return isNone
      
      bindingRet isGeneric

  of tyAnything:
    considerPreviousT:
      var concrete = concreteType(c, a)
      if concrete != nil and doBind:
        put(c.bindings, f, concrete)
      return isGeneric

  of tyBuiltInTypeClass:
    considerPreviousT:
      let targetKind = f.sons[0].kind
      if targetKind == a.skipTypes({tyRange, tyGenericInst}).kind or
         (targetKind in {tyProc, tyPointer} and a.kind == tyNil):
        put(c.bindings, f, a)
        return isGeneric
      else:
        return isNone

  of tyUserTypeClass, tyUserTypeClassInst:
    considerPreviousT:
      result = matchUserTypeClass(c.c, c, f, a)
      if result == isGeneric:
        put(c.bindings, f, a)

  of tyCompositeTypeClass:
    considerPreviousT:
      if typeRel(c, f.sons[1], a) != isNone:
        put(c.bindings, f, a)
        return isGeneric
      else:
        return isNone

  of tyGenericParam:
    var x = PType(idTableGet(c.bindings, f))
    if x == nil:
      if c.calleeSym != nil and c.calleeSym.kind == skType and
         f.kind == tyGenericParam and not c.typedescMatched:
        # XXX: The fact that generic types currently use tyGenericParam for 
        # their parameters is really a misnomer. tyGenericParam means "match
        # any value" and what we need is "match any type", which can be encoded
        # by a tyTypeDesc params. Unfortunately, this requires more substantial
        # changes in semtypinst and elsewhere.
        if a.kind == tyTypeDesc:
          if f.sonsLen == 0:
            result = isGeneric
          else:
            internalAssert a.sons != nil and a.sons.len > 0
            c.typedescMatched = true
            result = typeRel(c, f.sons[0], a.sons[0])
        else:
          result = isNone
      else:
        if f.sonsLen > 0 and f.sons[0].kind != tyNone:
          result = typeRel(c, f.lastSon, a)
        else:
          result = isGeneric

      if result == isGeneric:
        var concrete = concreteType(c, a)
        if concrete == nil:
          result = isNone
        else:
          if doBind: put(c.bindings, f, concrete)
    elif a.kind == tyEmpty:
      result = isGeneric
    elif x.kind == tyGenericParam:
      result = isGeneric
    else:
      result = typeRel(c, x, a) # check if it fits
  
  of tyStatic:
    if aOrig.kind == tyStatic:
      result = typeRel(c, f.lastSon, a)
      if result != isNone: put(c.bindings, f, aOrig)
    else:
      result = isNone

  of tyTypeDesc:
    var prev = PType(idTableGet(c.bindings, f))
    if prev == nil:
      if a.kind == tyTypeDesc:
        if f.sons[0].kind == tyNone:
          result = isGeneric
        else:
          result = typeRel(c, f.sons[0], a.sons[0])
        if result != isNone:
          put(c.bindings, f, a)
      else:
        result = isNone
    else:
      internalAssert prev.sonsLen == 1
      let toMatch = if tfUnresolved in f.flags: a
                    else: a.sons[0]
      result = typeRel(c, prev.sons[0], toMatch)
  
  of tyStmt:
    result = isGeneric
  
  of tyProxy:
    result = isEqual
  
  else: internalError("typeRel: " & $f.kind)
  
proc cmpTypes*(c: PContext, f, a: PType): TTypeRelation = 
  var m: TCandidate
  initCandidate(c, m, f)
  result = typeRel(m, f, a)

proc getInstantiatedType(c: PContext, arg: PNode, m: TCandidate, 
                         f: PType): PType = 
  result = PType(idTableGet(m.bindings, f))
  if result == nil: 
    result = generateTypeInstance(c, m.bindings, arg, f)
  if result == nil:
    internalError(arg.info, "getInstantiatedType")
    result = errorType(c)
  
proc implicitConv(kind: TNodeKind, f: PType, arg: PNode, m: TCandidate, 
                  c: PContext): PNode = 
  result = newNodeI(kind, arg.info)
  if containsGenericType(f):
    if not m.proxyMatch:
      result.typ = getInstantiatedType(c, arg, m, f)
    else:
      result.typ = errorType(c)
  else:
    result.typ = f
  if result.typ == nil: internalError(arg.info, "implicitConv")
  addSon(result, ast.emptyNode)
  addSon(result, arg)

proc userConvMatch(c: PContext, m: var TCandidate, f, a: PType, 
                   arg: PNode): PNode = 
  result = nil
  for i in countup(0, len(c.converters) - 1): 
    var src = c.converters[i].typ.sons[1]
    var dest = c.converters[i].typ.sons[0]
    # for generic type converters we need to check 'src <- a' before
    # 'f <- dest' in order to not break the unification:
    # see tests/tgenericconverter:
    let srca = typeRel(m, src, a)
    if srca notin {isEqual, isGeneric}: continue
    
    let destIsGeneric = containsGenericType(dest)
    if destIsGeneric:
      dest = generateTypeInstance(c, m.bindings, arg, dest)
    let fdest = typeRel(m, f, dest)
    if fdest in {isEqual, isGeneric}: 
      markUsed(arg, c.converters[i])
      var s = newSymNode(c.converters[i])
      s.typ = c.converters[i].typ
      s.info = arg.info
      result = newNodeIT(nkHiddenCallConv, arg.info, dest)
      addSon(result, s)
      addSon(result, copyTree(arg))
      inc(m.convMatches)
      m.genericConverter = srca == isGeneric or destIsGeneric
      return result

proc localConvMatch(c: PContext, m: var TCandidate, f, a: PType, 
                    arg: PNode): PNode = 
  # arg.typ can be nil in 'suggest':
  if isNil(arg.typ): return nil
  var call = newNodeI(nkCall, arg.info)
  call.add(f.n.copyTree)
  call.add(arg.copyTree)
  result = c.semOverloadedCall(c, call, call, routineKinds)
  if result != nil:
    # resulting type must be consistent with the other arguments:
    var r = typeRel(m, f.sons[0], result.typ)
    if r < isGeneric: return nil
    if result.kind == nkCall: result.kind = nkHiddenCallConv
    inc(m.convMatches)
    if r == isGeneric:
      result.typ = getInstantiatedType(c, arg, m, base(f))
    m.baseTypeMatch = true

proc paramTypesMatchAux(m: var TCandidate, f, argType: PType,
                        argSemantized, argOrig: PNode): PNode =
  var
    fMaybeStatic = f.skipTypes({tyDistinct})
    arg = argSemantized
    argType = argType
    c = m.c
    
  if tfHasStatic in fMaybeStatic.flags:
    # XXX: When implicit statics are the default
    # this will be done earlier - we just have to
    # make sure that static types enter here
    var evaluated = c.semTryConstExpr(c, arg)
    if evaluated != nil:
      arg.typ = newTypeS(tyStatic, c)
      arg.typ.sons = @[evaluated.typ]
      arg.typ.n = evaluated
      argType = arg.typ
 
  var
    a = if c.inTypeClass > 0: argType.skipTypes({tyTypeDesc})
        else: argType
 
    r = typeRel(m, f, a)

  case r
  of isConvertible:
    inc(m.convMatches)
    result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
  of isIntConv:
    # I'm too lazy to introduce another ``*matches`` field, so we conflate
    # ``isIntConv`` and ``isIntLit`` here:
    inc(m.intConvMatches)
    result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
  of isSubtype: 
    inc(m.subtypeMatches)
    result = implicitConv(nkHiddenSubConv, f, copyTree(arg), m, c)
  of isSubrange:
    inc(m.subtypeMatches)
    #result = copyTree(arg)
    result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
  of isInferred, isInferredConvertible:
    var prc = if arg.kind in nkLambdaKinds: arg[0].sym
              else: arg.sym
    let inferred = c.semGenerateInstance(c, prc, m.bindings, arg.info)
    result = newSymNode(inferred, arg.info)
    if r == isInferredConvertible:
      result = implicitConv(nkHiddenStdConv, f, result, m, c)
  of isGeneric:
    inc(m.genericMatches)
    if m.calleeSym != nil and m.calleeSym.kind in {skMacro, skTemplate}:
      if f.kind == tyStmt and argOrig.kind == nkDo:
        result = argOrig[bodyPos]
      elif f.kind == tyTypeDesc:
        result = arg
      elif f.kind == tyStatic:
        result = arg.typ.n
      else:
        result = argOrig
    else:
      result = copyTree(arg)
      result.typ = getInstantiatedType(c, arg, m, f)
      # BUG: f may not be the right key!
      if skipTypes(result.typ, abstractVar-{tyTypeDesc}).kind in {tyTuple}:
        result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
        # BUGFIX: use ``result.typ`` and not `f` here
  of isFromIntLit:
    # too lazy to introduce another ``*matches`` field, so we conflate
    # ``isIntConv`` and ``isIntLit`` here:
    inc(m.intConvMatches, 256)
    result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
  of isEqual: 
    inc(m.exactMatches)
    result = copyTree(arg)
    if skipTypes(f, abstractVar-{tyTypeDesc}).kind in {tyTuple}:
      result = implicitConv(nkHiddenStdConv, f, copyTree(arg), m, c)
  of isNone:
    # do not do this in ``typeRel`` as it then can't infere T in ``ref T``:
    if a.kind == tyProxy:
      inc(m.genericMatches)
      m.proxyMatch = true
      return copyTree(arg)
    result = userConvMatch(c, m, f, a, arg) 
    # check for a base type match, which supports varargs[T] without []
    # constructor in a call:
    if result == nil and f.kind == tyVarargs:
      if f.n != nil:
        result = localConvMatch(c, m, f, a, arg)
      else:
        r = typeRel(m, base(f), a)
        if r >= isGeneric:
          inc(m.convMatches)
          result = copyTree(arg)
          if r == isGeneric:
            result.typ = getInstantiatedType(c, arg, m, base(f))
          m.baseTypeMatch = true
        else:
          result = userConvMatch(c, m, base(f), a, arg)

proc paramTypesMatch*(m: var TCandidate, f, a: PType,
                      arg, argOrig: PNode): PNode =
  if arg == nil or arg.kind notin nkSymChoices:
    result = paramTypesMatchAux(m, f, a, arg, argOrig)
  else: 
    # CAUTION: The order depends on the used hashing scheme. Thus it is
    # incorrect to simply use the first fitting match. However, to implement
    # this correctly is inefficient. We have to copy `m` here to be able to
    # roll back the side effects of the unification algorithm.
    let c = m.c
    var x, y, z: TCandidate
    initCandidate(c, x, m.callee)
    initCandidate(c, y, m.callee)
    initCandidate(c, z, m.callee)
    x.calleeSym = m.calleeSym
    y.calleeSym = m.calleeSym
    z.calleeSym = m.calleeSym
    var best = -1
    for i in countup(0, sonsLen(arg) - 1): 
      if arg.sons[i].sym.kind in {skProc, skIterator, skMethod, skConverter}: 
        copyCandidate(z, m)
        var r = typeRel(z, f, arg.sons[i].typ)
        if r != isNone: 
          case x.state
          of csEmpty, csNoMatch: 
            x = z
            best = i
            x.state = csMatch
          of csMatch: 
            var cmp = cmpCandidates(x, z)
            if cmp < 0: 
              best = i
              x = z
            elif cmp == 0: 
              y = z           # z is as good as x
    if x.state == csEmpty: 
      result = nil
    elif (y.state == csMatch) and (cmpCandidates(x, y) == 0): 
      if x.state != csMatch: 
        internalError(arg.info, "x.state is not csMatch") 
      # ambiguous: more than one symbol fits
      result = nil
    else: 
      # only one valid interpretation found:
      markUsed(arg, arg.sons[best].sym)
      result = paramTypesMatchAux(m, f, arg.sons[best].typ, arg.sons[best],
                                  argOrig)

proc setSon(father: PNode, at: int, son: PNode) = 
  if sonsLen(father) <= at: setLen(father.sons, at + 1)
  father.sons[at] = son

# we are allowed to modify the calling node in the 'prepare*' procs:
proc prepareOperand(c: PContext; formal: PType; a: PNode): PNode =
  if formal.kind == tyExpr and formal.len != 1:
    # {tyTypeDesc, tyExpr, tyStmt, tyProxy}:
    # a.typ == nil is valid
    result = a
  elif a.typ.isNil:
    result = c.semOperand(c, a, {efDetermineType})
  else:
    result = a

proc prepareOperand(c: PContext; a: PNode): PNode =
  if a.typ.isNil:
    result = c.semOperand(c, a, {efDetermineType})
  else:
    result = a

proc prepareNamedParam(a: PNode) =
  if a.sons[0].kind != nkIdent:
    var info = a.sons[0].info
    a.sons[0] = newIdentNode(considerAcc(a.sons[0]), info)

proc arrayConstr(c: PContext, n: PNode): PType =
  result = newTypeS(tyArrayConstr, c)
  rawAddSon(result, makeRangeType(c, 0, 0, n.info))
  addSonSkipIntLit(result, skipTypes(n.typ, {tyGenericInst, tyVar, tyOrdinal}))

proc arrayConstr(c: PContext, info: TLineInfo): PType =
  result = newTypeS(tyArrayConstr, c)
  rawAddSon(result, makeRangeType(c, 0, -1, info))
  rawAddSon(result, newTypeS(tyEmpty, c)) # needs an empty basetype!

proc incrIndexType(t: PType) =
  assert t.kind == tyArrayConstr
  inc t.sons[0].n.sons[1].intVal

proc matchesAux(c: PContext, n, nOrig: PNode,
                m: var TCandidate, marker: var TIntSet) = 
  template checkConstraint(n: expr) {.immediate, dirty.} =
    if not formal.constraint.isNil:
      if matchNodeKinds(formal.constraint, n):
        # better match over other routines with no such restriction:
        inc(m.genericMatches, 100)
      else:
        m.state = csNoMatch
        return

  var
    # iterates over formal parameters
    f = if m.callee.kind != tyGenericBody: 1
        else: 0
    # iterates over the actual given arguments
    a = 1

  m.state = csMatch # until proven otherwise
  m.call = newNodeI(n.kind, n.info)
  m.call.typ = base(m.callee) # may be nil
  var formalLen = m.callee.n.len
  addSon(m.call, copyTree(n.sons[0]))
  var container: PNode = nil # constructed container
  var formal: PSym = nil

  while a < n.len:
    if n.sons[a].kind == nkExprEqExpr:
      # named param
      # check if m.callee has such a param:
      prepareNamedParam(n.sons[a])
      if n.sons[a].sons[0].kind != nkIdent: 
        localError(n.sons[a].info, errNamedParamHasToBeIdent)
        m.state = csNoMatch
        return 
      formal = getSymFromList(m.callee.n, n.sons[a].sons[0].ident, 1)
      if formal == nil: 
        # no error message!
        m.state = csNoMatch
        return 
      if containsOrIncl(marker, formal.position): 
        # already in namedParams:
        localError(n.sons[a].info, errCannotBindXTwice, formal.name.s)
        m.state = csNoMatch
        return 
      m.baseTypeMatch = false
      n.sons[a].sons[1] = prepareOperand(c, formal.typ, n.sons[a].sons[1])
      n.sons[a].typ = n.sons[a].sons[1].typ
      var arg = paramTypesMatch(m, formal.typ, n.sons[a].typ,
                                n.sons[a].sons[1], nOrig.sons[a].sons[1])
      if arg == nil:
        m.state = csNoMatch
        return
      checkConstraint(n.sons[a].sons[1])
      if m.baseTypeMatch: 
        assert(container == nil)
        container = newNodeIT(nkBracket, n.sons[a].info, arrayConstr(c, arg))
        addSon(container, arg)
        setSon(m.call, formal.position + 1, container)
        if f != formalLen - 1: container = nil
      else: 
        setSon(m.call, formal.position + 1, arg)
    else:
      # unnamed param
      if f >= formalLen:
        # too many arguments?
        if tfVarargs in m.callee.flags:
          # is ok... but don't increment any counters...
          # we have no formal here to snoop at:
          n.sons[a] = prepareOperand(c, n.sons[a])
          if skipTypes(n.sons[a].typ, abstractVar-{tyTypeDesc}).kind==tyString:
            addSon(m.call, implicitConv(nkHiddenStdConv, getSysType(tyCString),
                                        copyTree(n.sons[a]), m, c))
          else:
            addSon(m.call, copyTree(n.sons[a]))
        elif formal != nil:
          m.baseTypeMatch = false
          n.sons[a] = prepareOperand(c, formal.typ, n.sons[a])
          var arg = paramTypesMatch(m, formal.typ, n.sons[a].typ,
                                    n.sons[a], nOrig.sons[a])
          if (arg != nil) and m.baseTypeMatch and (container != nil):
            addSon(container, arg)
            incrIndexType(container.typ)
          else:
            m.state = csNoMatch
            return
        else:
          m.state = csNoMatch
          return
      else:
        if m.callee.n.sons[f].kind != nkSym: 
          internalError(n.sons[a].info, "matches")
          return
        formal = m.callee.n.sons[f].sym
        if containsOrIncl(marker, formal.position): 
          # already in namedParams:
          localError(n.sons[a].info, errCannotBindXTwice, formal.name.s)
          m.state = csNoMatch
          return 
        m.baseTypeMatch = false
        n.sons[a] = prepareOperand(c, formal.typ, n.sons[a])
        var arg = paramTypesMatch(m, formal.typ, n.sons[a].typ,
                                  n.sons[a], nOrig.sons[a])
        if arg == nil:
          m.state = csNoMatch
          return
        if m.baseTypeMatch:
          assert(container == nil)
          container = newNodeIT(nkBracket, n.sons[a].info, arrayConstr(c, arg))
          addSon(container, arg)
          setSon(m.call, formal.position + 1, 
                 implicitConv(nkHiddenStdConv, formal.typ, container, m, c))
          if f != formalLen - 1: container = nil
        else:
          setSon(m.call, formal.position + 1, arg)
      checkConstraint(n.sons[a])
    inc(a)
    inc(f)

proc semFinishOperands*(c: PContext, n: PNode) =
  # this needs to be called to ensure that after overloading resolution every
  # argument has been sem'checked:
  for i in 1 .. <n.len:
    n.sons[i] = prepareOperand(c, n.sons[i])

proc partialMatch*(c: PContext, n, nOrig: PNode, m: var TCandidate) =
  # for 'suggest' support:
  var marker = initIntSet()
  matchesAux(c, n, nOrig, m, marker)

proc matches*(c: PContext, n, nOrig: PNode, m: var TCandidate) =
  var marker = initIntSet()
  matchesAux(c, n, nOrig, m, marker)
  if m.state == csNoMatch: return
  # check that every formal parameter got a value:
  var f = 1
  while f < sonsLen(m.callee.n):
    var formal = m.callee.n.sons[f].sym
    if not containsOrIncl(marker, formal.position):
      if formal.ast == nil:
        if formal.typ.kind == tyVarargs:
          var container = newNodeIT(nkBracket, n.info, arrayConstr(c, n.info))
          addSon(m.call, implicitConv(nkHiddenStdConv, formal.typ,
                                      container, m, c))
        else:
          # no default value
          m.state = csNoMatch
          break
      else:
        # use default value:
        setSon(m.call, formal.position + 1, copyTree(formal.ast))
    inc(f)

proc argtypeMatches*(c: PContext, f, a: PType): bool =
  var m: TCandidate
  initCandidate(c, m, f)
  let res = paramTypesMatch(m, f, a, ast.emptyNode, nil)
  #instantiateGenericConverters(c, res, m)
  # XXX this is used by patterns.nim too; I think it's better to not
  # instantiate generic converters for that
  result = res != nil

include suggest

tests:
  var dummyOwner = newSym(skModule, getIdent("test_module"), nil, UnknownLineInfo())
  
  proc `|` (t1, t2: PType): PType =
    result = newType(tyOr, dummyOwner)
    result.rawAddSon(t1)
    result.rawAddSon(t2)

  proc `&` (t1, t2: PType): PType =
    result = newType(tyAnd, dummyOwner)
    result.rawAddSon(t1)
    result.rawAddSon(t2)

  proc `!` (t: PType): PType =
    result = newType(tyNot, dummyOwner)
    result.rawAddSon(t)

  proc seq(t: PType): PType =
    result = newType(tySequence, dummyOwner)
    result.rawAddSon(t)

  proc array(x: int, t: PType): PType =
    result = newType(tyArray, dummyOwner)
    
    var n = newNodeI(nkRange, UnknownLineInfo())
    addSon(n, newIntNode(nkIntLit, 0))
    addSon(n, newIntNode(nkIntLit, x))
    let range = newType(tyRange, dummyOwner)
    
    result.rawAddSon(range)
    result.rawAddSon(t)

  suite "type classes":
    let
      int = newType(tyInt, dummyOwner)
      float = newType(tyFloat, dummyOwner)
      string = newType(tyString, dummyOwner)
      ordinal = newType(tyOrdinal, dummyOwner)
      any = newType(tyAnything, dummyOwner)
      number = int | float

    var TFoo = newType(tyObject, dummyOwner)
    TFoo.sym = newSym(skType, getIdent"TFoo", dummyOwner, UnknownLineInfo())

    var T1 = newType(tyGenericParam, dummyOwner)
    T1.sym = newSym(skType, getIdent"T1", dummyOwner, UnknownLineInfo())
    T1.sym.position = 0

    var T2 = newType(tyGenericParam, dummyOwner)
    T2.sym = newSym(skType, getIdent"T2", dummyOwner, UnknownLineInfo())
    T2.sym.position = 1

    setup:
      var c: TCandidate
      InitCandidate(nil, c, nil)

    template yes(x, y) =
      test astToStr(x) & " is " & astToStr(y):
        check typeRel(c, y, x) == isGeneric

    template no(x, y) =
      test astToStr(x) & " is not " & astToStr(y):
        check typeRel(c, y, x) == isNone
    
    yes seq(any), array(10, int) | seq(any)
    # Sure, seq[any] is directly included

    yes seq(int), seq(any)
    yes seq(int), seq(number)
    # Sure, the int sequence is certainly
    # part of the number sequences (and all sequences)
    
    no seq(any), seq(float)
    # Nope, seq[any] includes types that are not seq[float] (e.g. seq[int])

    yes seq(int|string), seq(any)
    # Sure
 
    yes seq(int&string), seq(any)
    # Again
    
    yes seq(int&string), seq(int)
    # A bit more complicated
    # seq[int&string] is not a real type, but it's analogous to
    # seq[Sortable and Iterable], which is certainly a subset of seq[Sortable]

    no seq(int|string), seq(int|float)
    # Nope, seq[string] is not included in not included in
    # the seq[int|float] set
    
    no seq(!(int|string)), seq(string)
    # A sequence that is neither seq[int] or seq[string]
    # is obviously not seq[string]
     
    no seq(!int), seq(number)
    # Now your head should start to hurt a bit
    # A sequence that is not seq[int] is not necessarily a number sequence
    # it could well be seq[string] for example
    
    yes seq(!(int|string)), seq(!string)
    # all sequnece types besides seq[int] and seq[string]
    # are subset of all sequence types that are not seq[string]

    no seq(!(int|string)), seq(!(string|TFoo))
    # Nope, seq[TFoo] is included in the first set, but not in the second
    
    no seq(!string), seq(!number)
    # Nope, seq[int] in included in the first set, but not in the second

    yes seq(!number), seq(any)
    yes seq(!int), seq(any)
    no seq(any), seq(!any)
    no seq(!int), seq(!any)
    
    yes int, ordinal
    no  string, ordinal

