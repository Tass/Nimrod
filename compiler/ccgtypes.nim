#
#
#           The Nimrod Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

# ------------------------- Name Mangling --------------------------------

proc mangle(name: string): string = 
  case name[0]
  of 'a'..'z': 
    result = ""
    add(result, chr(ord(name[0]) - ord('a') + ord('A')))
  of '0'..'9', 'A'..'Z': 
    result = ""
    add(result, name[0])
  else: result = "HEX" & toHex(ord(name[0]), 2)
  for i in countup(0 + 1, len(name) + 0 - 1): 
    case name[i]
    of 'A'..'Z': 
      add(result, chr(ord(name[i]) - ord('A') + ord('a')))
    of '_': 
      nil
    of 'a'..'z', '0'..'9': 
      add(result, name[i])
    else: 
      add(result, "HEX")
      add(result, toHex(ord(name[i]), 2))

proc mangleName(s: PSym): PRope = 
  result = s.loc.r
  if result == nil: 
    if gCmd == cmdCompileToLLVM: 
      case s.kind
      of skProc, skMethod, skConverter, skConst: 
        result = toRope("@")
      of skVar, skForVar, skResult, skLet: 
        if sfGlobal in s.flags: result = toRope("@")
        else: result = toRope("%")
      of skTemp, skParam, skType, skEnumField, skModule: 
        result = toRope("%")
      else: InternalError(s.info, "mangleName")
    app(result, toRope(mangle(s.name.s)))
    app(result, "_")
    app(result, toRope(s.id))
    s.loc.r = result

proc isCompileTimeOnly(t: PType): bool =
  result = t.kind in {tyTypedesc, tyExpr}

proc getTypeName(typ: PType): PRope = 
  if (typ.sym != nil) and ({sfImportc, sfExportc} * typ.sym.flags != {}) and
      (gCmd != cmdCompileToLLVM): 
    result = typ.sym.loc.r
  else: 
    if typ.loc.r == nil: typ.loc.r = ropeff("TY$1", "%TY$1", [toRope(typ.id)])
    result = typ.loc.r
  if result == nil: InternalError("getTypeName: " & $typ.kind)
  
proc mapSetType(typ: PType): TCTypeKind =
  case int(getSize(typ))
  of 1: result = ctInt8
  of 2: result = ctInt16
  of 4: result = ctInt32
  of 8: result = ctInt64
  else: result = ctArray

proc mapType(typ: PType): TCTypeKind = 
  case typ.kind
  of tyNone: result = ctVoid
  of tyBool: result = ctBool
  of tyChar: result = ctChar
  of tySet: result = mapSetType(typ)
  of tyOpenArray, tyArrayConstr, tyArray: result = ctArray
  of tyObject, tyTuple: result = ctStruct
  of tyGenericBody, tyGenericInst, tyGenericParam, tyDistinct, tyOrdinal,
     tyConst, tyMutable, tyIter: 
    result = mapType(lastSon(typ))
  of tyEnum: 
    if firstOrd(typ) < 0: 
      result = ctInt32
    else: 
      case int(getSize(typ))
      of 1: result = ctUInt8
      of 2: result = ctUInt16
      of 4: result = ctInt32
      of 8: result = ctInt64
      else: internalError("mapType")
  of tyRange: result = mapType(typ.sons[0])
  of tyPtr, tyVar, tyRef: 
    var base = skipTypes(typ.sons[0], abstractInst)
    case base.kind
    of tyOpenArray, tyArrayConstr, tyArray: result = ctArray
    else: result = ctPtr
  of tyPointer: result = ctPtr
  of tySequence: result = ctNimSeq
  of tyProc: result = if typ.callConv != ccClosure: ctProc else: ctStruct
  of tyString: result = ctNimStr
  of tyCString: result = ctCString
  of tyInt..tyFloat128:
    result = TCTypeKind(ord(typ.kind) - ord(tyInt) + ord(ctInt))
  else: InternalError("mapType")
  
proc mapReturnType(typ: PType): TCTypeKind = 
  if skipTypes(typ, abstractInst).kind == tyArray: result = ctPtr
  else: result = mapType(typ)
  
proc getTypeDescAux(m: BModule, typ: PType, check: var TIntSet): PRope
proc needsComplexAssignment(typ: PType): bool = 
  result = containsGarbageCollectedRef(typ)

proc isInvalidReturnType(rettype: PType): bool = 
  # Arrays and sets cannot be returned by a C procedure, because C is
  # such a poor programming language.
  # We exclude records with refs too. This enhances efficiency and
  # is necessary for proper code generation of assignments.
  if rettype == nil: result = true
  else: 
    case mapType(rettype)
    of ctArray: 
      result = not (skipTypes(rettype, abstractInst).kind in
          {tyVar, tyRef, tyPtr})
    of ctStruct: 
      result = needsComplexAssignment(skipTypes(rettype, abstractInst))
    else: result = false
  
const 
  CallingConvToStr: array[TCallingConvention, string] = ["N_NIMCALL", 
    "N_STDCALL", "N_CDECL", "N_SAFECALL", 
    "N_SYSCALL", # this is probably not correct for all platforms,
                 # but one can #define it to what one wants 
    "N_INLINE", "N_NOINLINE", "N_FASTCALL", "N_CLOSURE", "N_NOCONV"]
  CallingConvToStrLLVM: array[TCallingConvention, string] = ["fastcc $1", 
    "stdcall $1", "ccc $1", "safecall $1", "syscall $1", "$1 alwaysinline", 
    "$1 noinline", "fastcc $1", "ccc $1", "$1"]

proc CacheGetType(tab: TIdTable, key: PType): PRope = 
  # returns nil if we need to declare this type
  # since types are now unique via the ``GetUniqueType`` mechanism, this slow
  # linear search is not necessary anymore:
  result = PRope(IdTableGet(tab, key))

proc getTempName(): PRope = 
  result = ropeff("TMP$1", "%TMP$1", [toRope(backendId())])

proc getGlobalTempName(): PRope = 
  result = ropeff("TMP$1", "@TMP$1", [toRope(backendId())])

proc ccgIntroducedPtr(s: PSym): bool = 
  var pt = skipTypes(s.typ, abstractInst)
  assert skResult != s.kind
  case pt.Kind
  of tyObject:
    if (optByRef in s.options) or (getSize(pt) > platform.floatSize * 2): 
      result = true           # requested anyway
    elif (tfFinal in pt.flags) and (pt.sons[0] == nil): 
      result = false          # no need, because no subtyping possible
    else: 
      result = true           # ordinary objects are always passed by reference,
                              # otherwise casting doesn't work
  of tyTuple: 
    result = (getSize(pt) > platform.floatSize*2) or (optByRef in s.options)
  else: result = false
  
proc fillResult(param: PSym) = 
  fillLoc(param.loc, locParam, param.typ, ropeff("Result", "%Result", []), 
          OnStack)
  if (mapReturnType(param.typ) != ctArray) and IsInvalidReturnType(param.typ): 
    incl(param.loc.flags, lfIndirect)
    param.loc.s = OnUnknown

proc getParamTypeDesc(m: BModule, t: PType, check: var TIntSet): PRope =
  when false:
    if t.Kind in {tyRef, tyPtr, tyVar}:
      var b = skipTypes(t.sons[0], abstractInst)
      if b.kind == tySet and mapSetType(b) == ctArray:
        return getTypeDescAux(m, b, check)
  result = getTypeDescAux(m, t, check)

proc genProcParams(m: BModule, t: PType, rettype, params: var PRope, 
                   check: var TIntSet, declareEnvironment=true) = 
  params = nil
  if (t.sons[0] == nil) or isInvalidReturnType(t.sons[0]): 
    rettype = toRope("void")
  else: 
    rettype = getTypeDescAux(m, t.sons[0], check)
  for i in countup(1, sonsLen(t.n) - 1): 
    if t.n.sons[i].kind != nkSym: InternalError(t.n.info, "genProcParams")
    var param = t.n.sons[i].sym
    if isCompileTimeOnly(param.typ): continue
    fillLoc(param.loc, locParam, param.typ, mangleName(param), OnStack)
    app(params, getParamTypeDesc(m, param.typ, check))
    if ccgIntroducedPtr(param): 
      app(params, "*")
      incl(param.loc.flags, lfIndirect)
      param.loc.s = OnUnknown
    app(params, " ")
    app(params, param.loc.r)
    # declare the len field for open arrays:
    var arr = param.typ
    if arr.kind == tyVar: arr = arr.sons[0]
    var j = 0
    while arr.Kind == tyOpenArray: 
      # need to pass hidden parameter:
      appff(params, ", NI $1Len$2", ", @NI $1Len$2", [param.loc.r, j.toRope])
      inc(j)
      arr = arr.sons[0]
    if i < sonsLen(t.n) - 1: app(params, ", ")
  if (t.sons[0] != nil) and isInvalidReturnType(t.sons[0]): 
    var arr = t.sons[0]
    if params != nil: app(params, ", ")
    app(params, getTypeDescAux(m, arr, check))
    if (mapReturnType(t.sons[0]) != ctArray) or (gCmd == cmdCompileToLLVM): 
      app(params, "*")
    appff(params, " Result", " @Result", [])
  if t.callConv == ccClosure and declareEnvironment: 
    if params != nil: app(params, ", ")
    app(params, "void* ClEnv")
  if tfVarargs in t.flags: 
    if params != nil: app(params, ", ")
    app(params, "...")
  if (params == nil) and (gCmd != cmdCompileToLLVM): app(params, "void)")
  else: app(params, ")")
  params = con("(", params)

proc isImportedType(t: PType): bool = 
  result = (t.sym != nil) and (sfImportc in t.sym.flags)

proc typeNameOrLiteral(t: PType, literal: string): PRope = 
  if (t.sym != nil) and (sfImportc in t.sym.flags) and (t.sym.magic == mNone): 
    result = getTypeName(t)
  else: 
    result = toRope(literal)
  
proc getSimpleTypeDesc(m: BModule, typ: PType): PRope = 
  const 
    NumericalTypeToStr: array[tyInt..tyFloat128, string] = ["NI", "NI8",
      "NI16", "NI32", "NI64", "NF", "NF32", "NF64", "NF128"]
  case typ.Kind
  of tyPointer: 
    result = typeNameOrLiteral(typ, "void*")
  of tyEnum: 
    if firstOrd(typ) < 0: 
      result = typeNameOrLiteral(typ, "NI32")
    else: 
      case int(getSize(typ))
      of 1: result = typeNameOrLiteral(typ, "NU8")
      of 2: result = typeNameOrLiteral(typ, "NU16")
      of 4: result = typeNameOrLiteral(typ, "NI32")
      of 8: result = typeNameOrLiteral(typ, "NI64")
      else: 
        internalError(typ.sym.info, "getSimpleTypeDesc: " & $(getSize(typ)))
        result = nil
  of tyString: 
    discard cgsym(m, "NimStringDesc")
    result = typeNameOrLiteral(typ, "NimStringDesc*")
  of tyCstring: result = typeNameOrLiteral(typ, "NCSTRING")
  of tyBool: result = typeNameOrLiteral(typ, "NIM_BOOL")
  of tyChar: result = typeNameOrLiteral(typ, "NIM_CHAR")
  of tyNil: result = typeNameOrLiteral(typ, "0")
  of tyInt..tyFloat128, tyUInt..tyUInt64: 
    result = typeNameOrLiteral(typ, NumericalTypeToStr[typ.Kind])
  of tyRange: result = getSimpleTypeDesc(m, typ.sons[0])
  else: result = nil
  
proc getTypePre(m: BModule, typ: PType): PRope = 
  if typ == nil: result = toRope("void")
  else: 
    result = getSimpleTypeDesc(m, typ)
    if result == nil: result = CacheGetType(m.typeCache, typ)
  
proc getForwardStructFormat(): string = 
  if gCmd == cmdCompileToCpp: result = "struct $1;$n"
  else: result = "typedef struct $1 $1;$n"
  
proc getTypeForward(m: BModule, typ: PType): PRope = 
  result = CacheGetType(m.forwTypeCache, typ)
  if result != nil: return 
  result = getTypePre(m, typ)
  if result != nil: return 
  case typ.kind
  of tySequence, tyTuple, tyObject: 
    result = getTypeName(typ)
    if not isImportedType(typ): 
      appf(m.s[cfsForwardTypes], getForwardStructFormat(), [result])
    IdTablePut(m.forwTypeCache, typ, result)
  else: InternalError("getTypeForward(" & $typ.kind & ')')
  
proc mangleRecFieldName(field: PSym, rectype: PType): PRope = 
  if (rectype.sym != nil) and
      ({sfImportc, sfExportc} * rectype.sym.flags != {}): 
    result = field.loc.r
  else: 
    result = toRope(mangle(field.name.s))
  if result == nil: InternalError(field.info, "mangleRecFieldName")
  
proc genRecordFieldsAux(m: BModule, n: PNode, 
                        accessExpr: PRope, rectype: PType, 
                        check: var TIntSet): PRope = 
  var 
    ae, uname, sname, a: PRope
    k: PNode
    field: PSym
  result = nil
  case n.kind
  of nkRecList: 
    for i in countup(0, sonsLen(n) - 1): 
      app(result, genRecordFieldsAux(m, n.sons[i], accessExpr, rectype, check))
  of nkRecCase: 
    if (n.sons[0].kind != nkSym): InternalError(n.info, "genRecordFieldsAux")
    app(result, genRecordFieldsAux(m, n.sons[0], accessExpr, rectype, check))
    uname = toRope(mangle(n.sons[0].sym.name.s) & 'U')
    if accessExpr != nil: ae = ropef("$1.$2", [accessExpr, uname])
    else: ae = uname
    app(result, "union {" & tnl)
    for i in countup(1, sonsLen(n) - 1): 
      case n.sons[i].kind
      of nkOfBranch, nkElse: 
        k = lastSon(n.sons[i])
        if k.kind != nkSym: 
          sname = con("S", toRope(i))
          a = genRecordFieldsAux(m, k, ropef("$1.$2", [ae, sname]), rectype, 
                                 check)
          if a != nil: 
            app(result, "struct {")
            app(result, a)
            appf(result, "} $1;$n", [sname])
        else: 
          app(result, genRecordFieldsAux(m, k, ae, rectype, check))
      else: internalError("genRecordFieldsAux(record case branch)")
    appf(result, "} $1;$n", [uname])
  of nkSym: 
    field = n.sym
    #assert(field.ast == nil)
    sname = mangleRecFieldName(field, rectype)
    if accessExpr != nil: ae = ropef("$1.$2", [accessExpr, sname])
    else: ae = sname
    fillLoc(field.loc, locField, field.typ, ae, OnUnknown)
    appf(result, "$1 $2;$n", [getTypeDescAux(m, field.loc.t, check), sname])
  else: internalError(n.info, "genRecordFieldsAux()")
  
proc getRecordFields(m: BModule, typ: PType, check: var TIntSet): PRope = 
  result = genRecordFieldsAux(m, typ.n, nil, typ, check)

proc getRecordDesc(m: BModule, typ: PType, name: PRope, 
                   check: var TIntSet): PRope = 
  # declare the record:
  var hasField = false
  if typ.kind == tyObject: 
    if typ.sons[0] == nil: 
      if (typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags: 
        result = ropecg(m, "struct $1 {$n", [name])
      else: 
        result = ropecg(m, "struct $1 {$n#TNimType* m_type;$n", [name])
        hasField = true
    elif gCmd == cmdCompileToCpp: 
      result = ropecg(m, "struct $1 : public $2 {$n", 
                      [name, getTypeDescAux(m, typ.sons[0], check)])
      hasField = true
    else: 
      result = ropecg(m, "struct $1 {$n  $2 Sup;$n", 
                      [name, getTypeDescAux(m, typ.sons[0], check)])
      hasField = true
  else: 
    result = ropef("struct $1 {$n", [name])
  var desc = getRecordFields(m, typ, check)
  if (desc == nil) and not hasField: 
    appf(result, "char dummy;$n", [])
  else: 
    app(result, desc)
  app(result, "};" & tnl)

proc getTupleDesc(m: BModule, typ: PType, name: PRope, 
                  check: var TIntSet): PRope = 
  result = ropef("struct $1 {$n", [name])
  var desc: PRope = nil
  for i in countup(0, sonsLen(typ) - 1): 
    appf(desc, "$1 Field$2;$n", 
         [getTypeDescAux(m, typ.sons[i], check), toRope(i)])
  if (desc == nil): app(result, "char dummy;" & tnl)
  else: app(result, desc)
  app(result, "};" & tnl)

proc pushType(m: BModule, typ: PType) = 
  add(m.typeStack, typ)

proc getTypeDescAux(m: BModule, typ: PType, check: var TIntSet): PRope = 
  # returns only the type's name
  var 
    name, rettype, desc, recdesc: PRope
    n: biggestInt
    t, et: PType
  t = getUniqueType(typ)
  if t == nil: InternalError("getTypeDescAux: t == nil")
  if t.sym != nil: useHeader(m, t.sym)
  result = getTypePre(m, t)
  if result != nil: return 
  if ContainsOrIncl(check, t.id): 
    InternalError("cannot generate C type for: " & typeToString(typ)) 
    # XXX: this BUG is hard to fix -> we need to introduce helper structs,
    # but determining when this needs to be done is hard. We should split
    # C type generation into an analysis and a code generation phase somehow.
  case t.Kind
  of tyRef, tyPtr, tyVar: 
    et = getUniqueType(t.sons[0])
    if et.kind in {tyArrayConstr, tyArray, tyOpenArray}: 
      # this is correct! sets have no proper base type, so we treat
      # ``var set[char]`` in `getParamTypeDesc`
      et = getUniqueType(elemType(et))
    case et.Kind
    of tyObject, tyTuple: 
      # no restriction! We have a forward declaration for structs
      name = getTypeForward(m, et)
      result = con(name, "*")
      IdTablePut(m.typeCache, t, result)
      pushType(m, et)
    of tySequence: 
      # no restriction! We have a forward declaration for structs
      name = getTypeForward(m, et)
      result = con(name, "**")
      IdTablePut(m.typeCache, t, result)
      pushType(m, et)
    else: 
      # else we have a strong dependency  :-(
      result = con(getTypeDescAux(m, et, check), "*")
      IdTablePut(m.typeCache, t, result)
  of tyOpenArray: 
    et = getUniqueType(t.sons[0])
    result = con(getTypeDescAux(m, et, check), "*")
    IdTablePut(m.typeCache, t, result)
  of tyProc: 
    result = getTypeName(t)
    IdTablePut(m.typeCache, t, result)
    genProcParams(m, t, rettype, desc, check)
    if not isImportedType(t): 
      if t.callConv != ccClosure: # procedure vars may need a closure!
        appf(m.s[cfsTypes], "typedef $1_PTR($2, $3) $4;$n", 
             [toRope(CallingConvToStr[t.callConv]), rettype, result, desc])
      else:
        appf(m.s[cfsTypes], "typedef struct {$n" &
            "N_NIMCALL_PTR($2, ClPrc) $3;$n" & 
            "void* ClEnv;$n} $1;$n",
             [result, rettype, desc])
  of tySequence: 
    # we cannot use getTypeForward here because then t would be associated
    # with the name of the struct, not with the pointer to the struct:
    result = CacheGetType(m.forwTypeCache, t)
    if result == nil: 
      result = getTypeName(t)
      if not isImportedType(t): 
        appf(m.s[cfsForwardTypes], getForwardStructFormat(), [result])
      IdTablePut(m.forwTypeCache, t, result)
    assert(CacheGetType(m.typeCache, t) == nil)
    IdTablePut(m.typeCache, t, con(result, "*"))
    if not isImportedType(t): 
      if skipTypes(t.sons[0], abstractInst).kind != tyEmpty: 
        appcg(m, m.s[cfsSeqTypes], 
            "struct $2 {$n" & 
            "  #TGenericSeq Sup;$n" &
            "  $1 data[SEQ_DECL_SIZE];$n" & 
            "};$n", [getTypeDescAux(m, t.sons[0], check), result])
      else: 
        result = toRope("TGenericSeq")
    app(result, "*")
  of tyArrayConstr, tyArray: 
    n = lengthOrd(t)
    if n <= 0: 
      n = 1                   # make an array of at least one element
    result = getTypeName(t)
    IdTablePut(m.typeCache, t, result)
    if not isImportedType(t): 
      appf(m.s[cfsTypes], "typedef $1 $2[$3];$n", 
           [getTypeDescAux(m, t.sons[1], check), result, ToRope(n)])
  of tyObject, tyTuple: 
    result = CacheGetType(m.forwTypeCache, t)
    if result == nil: 
      result = getTypeName(t)
      if not isImportedType(t): 
        appf(m.s[cfsForwardTypes], getForwardStructFormat(), [result])
      IdTablePut(m.forwTypeCache, t, result)
    IdTablePut(m.typeCache, t, result) # always call for sideeffects:
    if t.n != nil: recdesc = getRecordDesc(m, t, result, check)
    else: recdesc = getTupleDesc(m, t, result, check)
    if not isImportedType(t): app(m.s[cfsTypes], recdesc)
  of tySet: 
    case int(getSize(t))
    of 1: result = toRope("NU8")
    of 2: result = toRope("NU16")
    of 4: result = toRope("NU32")
    of 8: result = toRope("NU64")
    else: 
      result = getTypeName(t)
      IdTablePut(m.typeCache, t, result)
      if not isImportedType(t): 
        appf(m.s[cfsTypes], "typedef NU8 $1[$2];$n", 
             [result, toRope(getSize(t))])
  of tyGenericInst, tyDistinct, tyOrdinal, tyConst, tyMutable, tyIter: 
    result = getTypeDescAux(m, lastSon(t), check)
  else: 
    InternalError("getTypeDescAux(" & $t.kind & ')')
    result = nil

proc getTypeDesc(m: BModule, typ: PType): PRope = 
  var check = initIntSet()
  result = getTypeDescAux(m, typ, check)

type
  TClosureTypeKind = enum
    clHalf, clHalfWithEnv, clFull

proc getClosureType(m: BModule, t: PType, kind: TClosureTypeKind): PRope =
  assert t.kind == tyProc
  var check = initIntSet()
  result = getTempName()
  var rettype, desc: PRope
  genProcParams(m, t, rettype, desc, check, declareEnvironment=kind != clHalf)
  if not isImportedType(t):
    if t.callConv != ccClosure or kind != clFull:
      appf(m.s[cfsTypes], "typedef $1_PTR($2, $3) $4;$n", 
           [toRope(CallingConvToStr[t.callConv]), rettype, result, desc])
    else:
      appf(m.s[cfsTypes], "typedef struct {$n" &
          "N_NIMCALL_PTR($2, ClPrc) $3;$n" & 
          "void* ClEnv;$n} $1;$n",
           [result, rettype, desc])

proc getTypeDesc(m: BModule, magic: string): PRope = 
  var sym = magicsys.getCompilerProc(magic)
  if sym != nil: 
    result = getTypeDesc(m, sym.typ)
  else: 
    rawMessage(errSystemNeeds, magic)
    result = nil

proc finishTypeDescriptions(m: BModule) = 
  var i = 0
  while i < len(m.typeStack): 
    discard getTypeDesc(m, m.typeStack[i])
    inc(i)
  
proc genProcHeader(m: BModule, prc: PSym): PRope = 
  var 
    rettype, params: PRope
  genCLineDir(result, prc.info)
  # using static is needed for inline procs
  if gCmd != cmdCompileToLLVM and lfExportLib in prc.loc.flags:
    result.app "N_LIB_EXPORT "
  elif prc.typ.callConv == ccInline:
    result.app "static "
  var check = initIntSet()
  fillLoc(prc.loc, locProc, prc.typ, mangleName(prc), OnUnknown)
  genProcParams(m, prc.typ, rettype, params, check)
  # careful here! don't access ``prc.ast`` as that could reload large parts of
  # the object graph!
  appf(result, "$1($2, $3)$4", 
       [toRope(CallingConvToStr[prc.typ.callConv]), rettype, prc.loc.r, params])

# ------------------ type info generation -------------------------------------

proc genTypeInfo(m: BModule, typ: PType): PRope
proc getNimNode(m: BModule): PRope = 
  result = ropef("$1[$2]", [m.typeNodesName, toRope(m.typeNodes)])
  inc(m.typeNodes)

proc getNimType(m: BModule): PRope = 
  result = ropef("$1[$2]", [m.nimTypesName, toRope(m.nimTypes)])
  inc(m.nimTypes)

proc allocMemTI(m: BModule, typ: PType, name: PRope) = 
  var tmp = getNimType(m)
  appf(m.s[cfsTypeInit2], "$2 = &$1;$n", [tmp, name])

proc genTypeInfoAuxBase(m: BModule, typ: PType, name, base: PRope) = 
  var nimtypeKind: int
  allocMemTI(m, typ, name)
  if (typ.kind == tyObject) and (tfFinal in typ.flags) and
      (typ.sons[0] == nil): 
    nimtypeKind = ord(tyPureObject)
  else:
    nimtypeKind = ord(typ.kind)
  
  var size: PRope
  if tfIncompleteStruct in typ.flags: size = toRope"void*"
  else: size = getTypeDesc(m, typ)
  appf(m.s[cfsTypeInit3], 
       "$1->size = sizeof($2);$n" & "$1->kind = $3;$n" & "$1->base = $4;$n", 
       [name, size, toRope(nimtypeKind), base])     
  # compute type flags for GC optimization
  var flags = 0
  if not containsGarbageCollectedRef(typ): flags = flags or 1
  if not canFormAcycle(typ): flags = flags or 2        
  #else MessageOut("can contain a cycle: " & typeToString(typ))
  if flags != 0: 
    appf(m.s[cfsTypeInit3], "$1->flags = $2;$n", [name, toRope(flags)])
  appf(m.s[cfsVars], "TNimType* $1; /* $2 */$n", 
       [name, toRope(typeToString(typ))])

proc genTypeInfoAux(m: BModule, typ: PType, name: PRope) = 
  var base: PRope
  if (sonsLen(typ) > 0) and (typ.sons[0] != nil): 
    base = genTypeInfo(m, typ.sons[0])
  else: 
    base = toRope("0")
  genTypeInfoAuxBase(m, typ, name, base)

proc discriminatorTableName(m: BModule, objtype: PType, d: PSym): PRope = 
  if objType.sym == nil: 
    InternalError(d.info, "anonymous obj with discriminator")
  result = ropef("NimDT_$1_$2", [
    toRope(objType.sym.name.s), toRope(d.name.s)])

proc discriminatorTableDecl(m: BModule, objtype: PType, d: PSym): PRope = 
  discard cgsym(m, "TNimNode")
  var tmp = discriminatorTableName(m, objtype, d)
  result = ropef("TNimNode* $1[$2];$n", [tmp, toRope(lengthOrd(d.typ)+1)])

proc genObjectFields(m: BModule, typ: PType, n: PNode, expr: PRope) = 
  case n.kind
  of nkRecList: 
    var L = sonsLen(n)
    if L == 1: 
      genObjectFields(m, typ, n.sons[0], expr)
    elif L > 0: 
      var tmp = getTempName()
      appf(m.s[cfsTypeInit1], "static TNimNode* $1[$2];$n", [tmp, toRope(L)])
      for i in countup(0, L-1): 
        var tmp2 = getNimNode(m)
        appf(m.s[cfsTypeInit3], "$1[$2] = &$3;$n", [tmp, toRope(i), tmp2])
        genObjectFields(m, typ, n.sons[i], tmp2)
      appf(m.s[cfsTypeInit3], "$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n", 
           [expr, toRope(L), tmp])
    else:
      appf(m.s[cfsTypeInit3], "$1.len = $2; $1.kind = 2;$n", [expr, toRope(L)])
  of nkRecCase: 
    assert(n.sons[0].kind == nkSym)
    var field = n.sons[0].sym
    var tmp = discriminatorTableName(m, typ, field)
    var L = lengthOrd(field.typ)
    assert L > 0
    appf(m.s[cfsTypeInit3], "$1.kind = 3;$n" &
        "$1.offset = offsetof($2, $3);$n" & "$1.typ = $4;$n" &
        "$1.name = $5;$n" & "$1.sons = &$6[0];$n" &
        "$1.len = $7;$n", [expr, getTypeDesc(m, typ), field.loc.r, 
                           genTypeInfo(m, field.typ), 
                           makeCString(field.name.s), 
                           tmp, toRope(L)])
    appf(m.s[cfsData], "TNimNode* $1[$2];$n", [tmp, toRope(L+1)])
    for i in countup(1, sonsLen(n)-1): 
      var b = n.sons[i]           # branch
      var tmp2 = getNimNode(m)
      genObjectFields(m, typ, lastSon(b), tmp2)
      case b.kind
      of nkOfBranch: 
        if sonsLen(b) < 2: 
          internalError(b.info, "genObjectFields; nkOfBranch broken")
        for j in countup(0, sonsLen(b) - 2): 
          if b.sons[j].kind == nkRange: 
            var x = int(getOrdValue(b.sons[j].sons[0]))
            var y = int(getOrdValue(b.sons[j].sons[1]))
            while x <= y: 
              appf(m.s[cfsTypeInit3], "$1[$2] = &$3;$n", [tmp, toRope(x), tmp2])
              inc(x)
          else: 
            appf(m.s[cfsTypeInit3], "$1[$2] = &$3;$n", 
                 [tmp, toRope(getOrdValue(b.sons[j])), tmp2])
      of nkElse: 
        appf(m.s[cfsTypeInit3], "$1[$2] = &$3;$n", 
             [tmp, toRope(L), tmp2])
      else: internalError(n.info, "genObjectFields(nkRecCase)")
  of nkSym: 
    var field = n.sym
    appf(m.s[cfsTypeInit3], "$1.kind = 1;$n" &
        "$1.offset = offsetof($2, $3);$n" & "$1.typ = $4;$n" &
        "$1.name = $5;$n", [expr, getTypeDesc(m, typ), 
        field.loc.r, genTypeInfo(m, field.typ), makeCString(field.name.s)])
  else: internalError(n.info, "genObjectFields")
  
proc genObjectInfo(m: BModule, typ: PType, name: PRope) = 
  if typ.kind == tyObject: genTypeInfoAux(m, typ, name)
  else: genTypeInfoAuxBase(m, typ, name, toRope("0"))
  var tmp = getNimNode(m)
  genObjectFields(m, typ, typ.n, tmp)
  appf(m.s[cfsTypeInit3], "$1->node = &$2;$n", [name, tmp])

proc genTupleInfo(m: BModule, typ: PType, name: PRope) =
  genTypeInfoAuxBase(m, typ, name, toRope("0"))
  var expr = getNimNode(m)
  var length = sonsLen(typ)
  if length > 0: 
    var tmp = getTempName()
    appf(m.s[cfsTypeInit1], "static TNimNode* $1[$2];$n", [tmp, toRope(length)])
    for i in countup(0, length - 1): 
      var a = typ.sons[i]
      var tmp2 = getNimNode(m)
      appf(m.s[cfsTypeInit3], "$1[$2] = &$3;$n", [tmp, toRope(i), tmp2])
      appf(m.s[cfsTypeInit3], "$1.kind = 1;$n" &
          "$1.offset = offsetof($2, Field$3);$n" & 
          "$1.typ = $4;$n" &
          "$1.name = \"Field$3\";$n", 
           [tmp2, getTypeDesc(m, typ), toRope(i), genTypeInfo(m, a)])
    appf(m.s[cfsTypeInit3], "$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n", 
         [expr, toRope(length), tmp])
  else: 
    appf(m.s[cfsTypeInit3], "$1.len = $2; $1.kind = 2;$n", 
         [expr, toRope(length)])
  appf(m.s[cfsTypeInit3], "$1->node = &$2;$n", [name, expr])

proc genEnumInfo(m: BModule, typ: PType, name: PRope) =
  # Type information for enumerations is quite heavy, so we do some
  # optimizations here: The ``typ`` field is never set, as it is redundant
  # anyway. We generate a cstring array and a loop over it. Exceptional
  # positions will be reset after the loop.
  genTypeInfoAux(m, typ, name)
  var nodePtrs = getTempName()
  var length = sonsLen(typ.n)
  appf(m.s[cfsTypeInit1], "static TNimNode* $1[$2];$n", 
       [nodePtrs, toRope(length)])
  var enumNames, specialCases: PRope
  var firstNimNode = m.typeNodes
  var hasHoles = false
  for i in countup(0, length - 1): 
    assert(typ.n.sons[i].kind == nkSym)
    var field = typ.n.sons[i].sym
    var elemNode = getNimNode(m)
    if field.ast == nil:
      # no explicit string literal for the enum field, so use field.name:
      app(enumNames, makeCString(field.name.s))
    else:
      app(enumNames, makeCString(field.ast.strVal))
    if i < length - 1: app(enumNames, ", " & tnl)
    if field.position != i or tfEnumHasHoles in typ.flags:
      appf(specialCases, "$1.offset = $2;$n", [elemNode, toRope(field.position)])
      hasHoles = true
  var enumArray = getTempName()
  var counter = getTempName()
  appf(m.s[cfsTypeInit1], "NI $1;$n", [counter])
  appf(m.s[cfsTypeInit1], "static char* NIM_CONST $1[$2] = {$n$3};$n", 
       [enumArray, toRope(length), enumNames])
  appf(m.s[cfsTypeInit3], "for ($1 = 0; $1 < $2; $1++) {$n" &
      "$3[$1+$4].kind = 1;$n" & "$3[$1+$4].offset = $1;$n" &
      "$3[$1+$4].name = $5[$1];$n" & "$6[$1] = &$3[$1+$4];$n" & "}$n", [counter, 
      toRope(length), m.typeNodesName, toRope(firstNimNode), enumArray, nodePtrs])
  app(m.s[cfsTypeInit3], specialCases)
  appf(m.s[cfsTypeInit3], 
       "$1.len = $2; $1.kind = 2; $1.sons = &$3[0];$n$4->node = &$1;$n", 
       [getNimNode(m), toRope(length), nodePtrs, name])
  if hasHoles:
    # 1 << 2 is {ntfEnumHole}
    appf(m.s[cfsTypeInit3], "$1->flags = 1<<2;$n", [name])

proc genSetInfo(m: BModule, typ: PType, name: PRope) = 
  assert(typ.sons[0] != nil)
  genTypeInfoAux(m, typ, name)
  var tmp = getNimNode(m)
  appf(m.s[cfsTypeInit3], "$1.len = $2; $1.kind = 0;$n" & "$3->node = &$1;$n", 
       [tmp, toRope(firstOrd(typ)), name])

proc genArrayInfo(m: BModule, typ: PType, name: PRope) = 
  genTypeInfoAuxBase(m, typ, name, genTypeInfo(m, typ.sons[1]))

proc fakeClosureType(owner: PSym): PType =
  # we generate the same RTTI as for a tuple[pointer, ref tuple[]]
  result = newType(tyTuple, owner)
  result.addSon(newType(tyPointer, owner))
  var r = newType(tyRef, owner)
  r.addSon(newType(tyTuple, owner))
  result.addSon(r)

type
  TTypeInfoReason = enum  ## for what do we need the type info?
    tiNew,                ## for 'new'
    tiNewSeq,             ## for 'newSeq'
    tiNonVariantAsgn,     ## for generic assignment without variants
    tiVariantAsgn         ## for generic assignment with variants

include ccgtrav

proc genTypeInfo(m: BModule, typ: PType): PRope = 
  var t = getUniqueType(typ)
  # gNimDat contains all the type information nowadays:
  var dataGenerated = ContainsOrIncl(gNimDat.typeInfoMarker, t.id)
  result = ropef("NTI$1", [toRope(t.id)])
  if not ContainsOrIncl(m.typeInfoMarker, t.id): 
    # declare type information structures:
    discard cgsym(m, "TNimType")
    discard cgsym(m, "TNimNode")
    appf(m.s[cfsVars], "extern TNimType* $1; /* $2 */$n", 
         [result, toRope(typeToString(t))])
  if dataGenerated: return 
  case t.kind
  of tyEmpty: result = toRope"0"
  of tyPointer, tyBool, tyChar, tyCString, tyString, tyInt..tyFloat128, tyVar:
    genTypeInfoAuxBase(gNimDat, t, result, toRope"0")
  of tyProc:
    if t.callConv != ccClosure:
      genTypeInfoAuxBase(gNimDat, t, result, toRope"0")
    else:
      genTupleInfo(gNimDat, fakeClosureType(t.owner), result)
  of tySequence, tyRef:
    genTypeInfoAux(gNimDat, t, result)
    if optRefcGC in gGlobalOptions:
      let markerProc = genTraverseProc(gNimDat, t, tiNew)
      appf(gNimDat.s[cfsTypeInit3], "$1->marker = $2;$n", [result, markerProc])
  of tyPtr, tyRange: genTypeInfoAux(gNimDat, t, result)
  of tyArrayConstr, tyArray: genArrayInfo(gNimDat, t, result)
  of tySet: genSetInfo(gNimDat, t, result)
  of tyEnum: genEnumInfo(gNimDat, t, result)
  of tyObject: genObjectInfo(gNimDat, t, result)
  of tyTuple: 
    if t.n != nil: genObjectInfo(gNimDat, t, result)
    else: genTupleInfo(gNimDat, t, result)
  else: InternalError("genTypeInfo(" & $t.kind & ')')

proc genTypeSection(m: BModule, n: PNode) = 
  nil
