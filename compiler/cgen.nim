#
#
#           The Nimrod Compiler
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This is the new C code generator; much cleaner and faster
# than the old one. It also generates better code.

import 
  ast, astalgo, strutils, hashes, trees, platform, magicsys, extccomp,
  options, intsets,
  nversion, nimsets, msgs, crc, bitsets, idents, lists, types, ccgutils, os, 
  times, ropes, math, passes, rodread, wordrecg, treetab, cgmeth,
  rodutils, renderer

when options.hasTinyCBackend:
  import tccgen

proc cgenPass*(): TPass
# implementation

type 
  TLabel = PRope              # for the C generator a label is just a rope
  TCFileSection = enum        # the sections a generated C file consists of
    cfsHeaders,               # section for C include file headers
    cfsForwardTypes,          # section for C forward typedefs
    cfsTypes,                 # section for C typedefs
    cfsSeqTypes,              # section for sequence types only
                              # this is needed for strange type generation
                              # reasons
    cfsFieldInfo,             # section for field information
    cfsTypeInfo,              # section for type information
    cfsProcHeaders,           # section for C procs prototypes
    cfsData,                  # section for C constant data
    cfsVars,                  # section for C variable declarations
    cfsProcs,                 # section for C procs that are not inline
    cfsTypeInit1,             # section 1 for declarations of type information
    cfsTypeInit2,             # section 2 for init of type information
    cfsTypeInit3,             # section 3 for init of type information
    cfsDebugInit,             # section for init of debug information
    cfsDynLibInit,            # section for init of dynamic library binding
    cfsDynLibDeinit           # section for deinitialization of dynamic
                              # libraries
  TCTypeKind = enum           # describes the type kind of a C type
    ctVoid, ctChar, ctBool, ctUInt, ctUInt8, ctUInt16, ctUInt32, ctUInt64, 
    ctInt, ctInt8, ctInt16, ctInt32, ctInt64, ctFloat, ctFloat32, ctFloat64, 
    ctFloat128, ctArray, ctStruct, ctPtr, ctNimStr, ctNimSeq, ctProc, ctCString
  TCFileSections = array[TCFileSection, PRope] # represents a generated C file
  TCProcSection = enum        # the sections a generated C proc consists of
    cpsLocals,                # section of local variables for C proc
    cpsInit,                  # section for init of variables for C proc
    cpsStmts                  # section of local statements for C proc
  TCProcSections = array[TCProcSection, PRope] # represents a generated C proc
  BModule = ref TCGen
  BProc = ref TCProc
  TBlock{.final.} = object 
    id*: int                  # the ID of the label; positive means that it
                              # has been used (i.e. the label should be emitted)
    nestedTryStmts*: int      # how many try statements is it nested into
  
  TCProc{.final.} = object   # represents C proc that is currently generated
    s: TCProcSections        # the procs sections; short name for readability
    prc: PSym                # the Nimrod proc that this C proc belongs to
    BeforeRetNeeded: bool    # true iff 'BeforeRet' label for proc is needed
    ThreadVarAccessed: bool  # true if the proc already accessed some threadvar
    nestedTryStmts: seq[PNode] # in how many nested try statements we are
                               # (the vars must be volatile then)
    labels: Natural          # for generating unique labels in the C proc
    blocks: seq[TBlock]      # nested blocks
    options: TOptions        # options that should be used for code
                             # generation; this is the same as prc.options
                             # unless prc == nil
    frameLen: int            # current length of frame descriptor
    sendClosure: PType       # closure record type that we pass
    receiveClosure: PType    # closure record type that we get
    module: BModule          # used to prevent excessive parameter passing
    withinLoop: int          # > 0 if we are within a loop
  
  TTypeSeq = seq[PType]
  TCGen = object of TPassContext # represents a C source file
    module*: PSym
    filename*: string
    s*: TCFileSections        # sections of the C file
    PreventStackTrace: bool   # true if stack traces need to be prevented
    usesThreadVars: bool      # true if the module uses a thread var
    cfilename*: string        # filename of the module (including path,
                              # without extension)
    typeCache*: TIdTable      # cache the generated types
    forwTypeCache*: TIdTable  # cache for forward declarations of types
    declaredThings*: TIntSet  # things we have declared in this .c file
    declaredProtos*: TIntSet  # prototypes we have declared in this .c file
    headerFiles*: TLinkedList # needed headers to include
    typeInfoMarker*: TIntSet  # needed for generating type information
    initProc*: BProc          # code for init procedure
    typeStack*: TTypeSeq      # used for type generation
    dataCache*: TNodeTable
    forwardedProcs*: TSymSeq  # keep forwarded procs here
    typeNodes*, nimTypes*: int # used for type info generation
    typeNodesName*, nimTypesName*: PRope # used for type info generation
    labels*: natural          # for generating unique module-scope names

var 
  mainModProcs, mainModInit: PRope # parts of the main module
  gMapping: PRope             # the generated mapping file (if requested)
  gProcProfile: Natural       # proc profile counter
  gGeneratedSyms: TIntSet     # set of ID's of generated symbols
  gPendingModules: seq[BModule] = @[] # list of modules that are not
                                      # finished with code generation
  gForwardedProcsCounter: int = 0
  gNimDat: BModule            # generated global data

proc ropeff(cformat, llvmformat: string, args: openarray[PRope]): PRope = 
  if gCmd == cmdCompileToLLVM: result = ropef(llvmformat, args)
  else: result = ropef(cformat, args)
  
proc appff(dest: var PRope, cformat, llvmformat: string, 
           args: openarray[PRope]) = 
  if gCmd == cmdCompileToLLVM: appf(dest, llvmformat, args)
  else: appf(dest, cformat, args)
  
proc addForwardedProc(m: BModule, prc: PSym) = 
  m.forwardedProcs.add(prc)
  inc(gForwardedProcsCounter)

proc addPendingModule(m: BModule) = 
  for i in countup(0, high(gPendingModules)): 
    if gPendingModules[i] == m: 
      InternalError("module already pending: " & m.module.name.s)
  gPendingModules.add(m)

proc findPendingModule(m: BModule, s: PSym): BModule = 
  var ms = getModule(s)
  if ms.id == m.module.id: return m
  for i in countup(0, high(gPendingModules)): 
    result = gPendingModules[i]
    if result.module.id == ms.id: return 
  InternalError(s.info, "no pending module found for: " & s.name.s)

proc initLoc(result: var TLoc, k: TLocKind, typ: PType, s: TStorageLoc) = 
  result.k = k
  result.s = s
  result.t = GetUniqueType(typ)
  result.r = nil
  result.a = - 1
  result.flags = {}

proc fillLoc(a: var TLoc, k: TLocKind, typ: PType, r: PRope, s: TStorageLoc) = 
  # fills the loc if it is not already initialized
  if a.k == locNone: 
    a.k = k
    a.t = getUniqueType(typ)
    a.a = - 1
    a.s = s
    if a.r == nil: a.r = r
  
proc newProc(prc: PSym, module: BModule): BProc = 
  new(result)
  result.prc = prc
  result.module = module
  if prc != nil: result.options = prc.options
  else: result.options = gOptions
  result.blocks = @[]
  result.nestedTryStmts = @[]

proc isSimpleConst(typ: PType): bool = 
  result = not (skipTypes(typ, abstractVar).kind in
      {tyTuple, tyObject, tyArray, tyArrayConstr, tySet, tySequence})

proc useHeader(m: BModule, sym: PSym) = 
  if lfHeader in sym.loc.Flags: 
    assert(sym.annex != nil)
    discard lists.IncludeStr(m.headerFiles, getStr(sym.annex.path))

proc cgsym(m: BModule, name: string): PRope

proc ropecg(m: BModule, frmt: TFormatStr, args: openarray[PRope]): PRope = 
  var i, j, length, start, num: int
  i = 0
  length = len(frmt)
  result = nil
  num = 0
  while i < length: 
    if frmt[i] == '$': 
      inc(i)                  # skip '$'
      case frmt[i]
      of '$': 
        app(result, "$")
        inc(i)
      of '#': 
        inc(i)
        app(result, args[num])
        inc(num)
      of '0'..'9': 
        j = 0
        while true: 
          j = (j * 10) + Ord(frmt[i]) - ord('0')
          inc(i)
          if i >= length or not (frmt[i] in {'0'..'9'}): break 
        num = j
        if j > high(args) + 1: 
          internalError("ropes: invalid format string $" & $(j))
        app(result, args[j - 1])
      of 'N', 'n': 
        app(result, tnl)
        inc(i)
      else: InternalError("ropes: invalid format string $" & frmt[i])
    elif frmt[i] == '#' and frmt[i+1] in IdentStartChars:
      inc(i)
      var j = i
      while frmt[j] in IdentChars: inc(j)
      var ident = substr(frmt, i, j-1)
      i = j
      app(result, cgsym(m, ident))
    elif frmt[i] == '#' and frmt[i+1] == '$':
      inc(i, 2)
      var j = 0
      while frmt[i] in Digits: 
        j = (j * 10) + Ord(frmt[i]) - ord('0')
        inc(i)
      app(result, cgsym(m, args[j-1].ropeToStr))
    start = i
    while i < length: 
      if frmt[i] != '$' and frmt[i] != '#': inc(i)
      else: break 
    if i - 1 >= start: 
      app(result, substr(frmt, start, i - 1))

proc appcg(m: BModule, c: var PRope, frmt: TFormatStr, 
           args: openarray[PRope]) = 
  app(c, ropecg(m, frmt, args))

proc appcg(m: BModule, s: TCFileSection, frmt: TFormatStr, 
           args: openarray[PRope]) = 
  app(m.s[s], ropecg(m, frmt, args))

proc appcg(p: BProc, s: TCProcSection, frmt: TFormatStr, 
           args: openarray[PRope]) = 
  app(p.s[s], ropecg(p.module, frmt, args))


include "ccgtypes.nim"

# ------------------------------ Manager of temporaries ------------------

proc rdLoc(a: TLoc): PRope =
  # 'read' location (deref if indirect)
  result = a.r
  if lfIndirect in a.flags: result = ropef("(*$1)", [result])

proc addrLoc(a: TLoc): PRope =
  result = a.r
  if lfIndirect notin a.flags and mapType(a.t) != ctArray: 
    result = con("&", result)

proc rdCharLoc(a: TLoc): PRope =
  # read a location that may need a char-cast:
  result = rdLoc(a)
  if skipTypes(a.t, abstractRange).kind == tyChar:
    result = ropef("((NU8)($1))", [result])

proc genObjectInit(p: BProc, section: TCProcSection, t: PType, a: TLoc, 
                   takeAddr: bool) =
  case analyseObjectWithTypeField(t)
  of frNone:
    nil
  of frHeader:
    var r = rdLoc(a)
    if not takeAddr: r = ropef("(*$1)", [r])
    var s = skipTypes(t, abstractInst)
    while (s.kind == tyObject) and (s.sons[0] != nil):
      app(r, ".Sup")
      s = skipTypes(s.sons[0], abstractInst)
    appcg(p, section, "$1.m_type = $2;$n", [r, genTypeInfo(p.module, t)])
  of frEmbedded:
    # worst case for performance:
    var r = if takeAddr: addrLoc(a) else: rdLoc(a)
    appcg(p, section, "#objectInit($1, $2);$n", [r, genTypeInfo(p.module, t)])

type
  TAssignmentFlag = enum
    needToCopy, needForSubtypeCheck, afDestIsNil, afDestIsNotNil, afSrcIsNil,
    afSrcIsNotNil
  TAssignmentFlags = set[TAssignmentFlag]

proc genRefAssign(p: BProc, dest, src: TLoc, flags: TAssignmentFlags)

proc zeroVar(p: BProc, loc: TLoc, containsGCref: bool) = 
  if skipTypes(loc.t, abstractVarRange).Kind notin
      {tyArray, tyArrayConstr, tySet, tyTuple, tyObject}: 
    if containsGcref and p.WithInLoop > 0:
      appf(p.s[cpsInit], "$1 = 0;$n", [rdLoc(loc)])
      var nilLoc: TLoc
      initLoc(nilLoc, locTemp, loc.t, onStack)
      nilLoc.r = toRope("NIM_NIL")
      # puts ``unsureAsgnRef`` etc to ``p.s[cpsStmts]``:
      genRefAssign(p, loc, nilLoc, {afSrcIsNil})
    else:
      appf(p.s[cpsStmts], "$1 = 0;$n", [rdLoc(loc)])
  else: 
    if containsGcref and p.WithInLoop > 0:
      appf(p.s[cpsInit], "memset((void*)$1, 0, sizeof($2));$n", 
           [addrLoc(loc), rdLoc(loc)])
      appcg(p, cpsStmts, "#genericReset((void*)$1, $2);$n", 
           [addrLoc(loc), genTypeInfo(p.module, loc.t)])
    else:
      appf(p.s[cpsStmts], "memset((void*)$1, 0, sizeof($2));$n", 
           [addrLoc(loc), rdLoc(loc)])
    genObjectInit(p, cpsInit, loc.t, loc, true)

proc zeroTemp(p: BProc, loc: TLoc) = 
  if skipTypes(loc.t, abstractVarRange).Kind notin
      {tyArray, tyArrayConstr, tySet, tyTuple, tyObject}: 
    appf(p.s[cpsStmts], "$1 = 0;$n", [rdLoc(loc)])
    when false:
      var nilLoc: TLoc
      initLoc(nilLoc, locTemp, loc.t, onStack)
      nilLoc.r = toRope("NIM_NIL")
      # puts ``unsureAsgnRef`` etc to ``p.s[cpsStmts]``:
      genRefAssign(p, loc, nilLoc, {afSrcIsNil})
  else: 
    appf(p.s[cpsStmts], "memset((void*)$1, 0, sizeof($2));$n", 
         [addrLoc(loc), rdLoc(loc)])
    when false:
      appcg(p, cpsStmts, "#genericReset((void*)$1, $2);$n", 
           [addrLoc(loc), genTypeInfo(p.module, loc.t)])

proc initVariable(p: BProc, v: PSym) = 
  var b = containsGarbageCollectedRef(v.typ)
  if b or v.ast == nil: 
    zeroVar(p, v.loc, b)
    
proc initTemp(p: BProc, tmp: var TLoc) = 
  if containsGarbageCollectedRef(tmp.t) or isInvalidReturnType(tmp.t):
    zeroTemp(p, tmp)

proc getTemp(p: BProc, t: PType, result: var TLoc) = 
  inc(p.labels)
  if gCmd == cmdCompileToLLVM: 
    result.r = con("%LOC", toRope(p.labels))
  else: 
    result.r = con("LOC", toRope(p.labels))
    appf(p.s[cpsLocals], "$1 $2;$n", [getTypeDesc(p.module, t), result.r])
  result.k = locTemp
  result.a = - 1
  result.t = getUniqueType(t)
  result.s = OnStack
  result.flags = {}
  initTemp(p, result)

proc cstringLit(p: BProc, r: var PRope, s: string): PRope = 
  if gCmd == cmdCompileToLLVM: 
    inc(p.module.labels)
    inc(p.labels)
    result = ropef("%LOC$1", [toRope(p.labels)])
    appf(p.module.s[cfsData], "@C$1 = private constant [$2 x i8] $3$n", 
         [toRope(p.module.labels), toRope(len(s)), makeLLVMString(s)])
    appf(r, "$1 = getelementptr [$2 x i8]* @C$3, %NI 0, %NI 0$n", 
         [result, toRope(len(s)), toRope(p.module.labels)])
  else: 
    result = makeCString(s)
  
proc cstringLit(m: BModule, r: var PRope, s: string): PRope = 
  if gCmd == cmdCompileToLLVM: 
    inc(m.labels, 2)
    result = ropef("%MOC$1", [toRope(m.labels - 1)])
    appf(m.s[cfsData], "@MOC$1 = private constant [$2 x i8] $3$n", 
         [toRope(m.labels), toRope(len(s)), makeLLVMString(s)])
    appf(r, "$1 = getelementptr [$2 x i8]* @MOC$3, %NI 0, %NI 0$n", 
         [result, toRope(len(s)), toRope(m.labels)])
  else: 
    result = makeCString(s)
  
proc allocParam(p: BProc, s: PSym) = 
  assert(s.kind == skParam)
  if lfParamCopy notin s.loc.flags: 
    inc(p.labels)
    var tmp = con("%LOC", toRope(p.labels))
    incl(s.loc.flags, lfParamCopy)
    incl(s.loc.flags, lfIndirect)
    appf(p.s[cpsInit], "$1 = alloca $3$n" & "store $3 $2, $3* $1$n", 
         [tmp, s.loc.r, getTypeDesc(p.module, s.loc.t)])
    s.loc.r = tmp

proc localDebugInfo(p: BProc, s: PSym) = 
  if {optStackTrace, optEndb} * p.options != {optStackTrace, optEndb}: return 
  # XXX work around a bug: No type information for open arrays possible:
  if skipTypes(s.typ, abstractVar).kind == tyOpenArray: return
  var a = con("&", s.loc.r)
  if (s.kind == skParam) and ccgIntroducedPtr(s): a = s.loc.r
  appf(p.s[cpsInit], 
       "F.s[$1].address = (void*)$3; F.s[$1].typ = $4; F.s[$1].name = $2;$n",
       [toRope(p.frameLen), makeCString(normalize(s.name.s)), a, 
        genTypeInfo(p.module, s.loc.t)])
  inc(p.frameLen)

proc assignLocalVar(p: BProc, s: PSym) = 
  #assert(s.loc.k == locNone) // not yet assigned
  # this need not be fullfilled for inline procs; they are regenerated
  # for each module that uses them!
  if s.loc.k == locNone: 
    fillLoc(s.loc, locLocalVar, s.typ, mangleName(s), OnStack)
  app(p.s[cpsLocals], getTypeDesc(p.module, s.loc.t))
  if sfRegister in s.flags: app(p.s[cpsLocals], " register")
  if (sfVolatile in s.flags) or (p.nestedTryStmts.len > 0): 
    app(p.s[cpsLocals], " volatile")
  appf(p.s[cpsLocals], " $1;$n", [s.loc.r])
  localDebugInfo(p, s)

include ccgthreadvars

proc assignGlobalVar(p: BProc, s: PSym) = 
  if s.loc.k == locNone: 
    fillLoc(s.loc, locGlobalVar, s.typ, mangleName(s), OnHeap)
  useHeader(p.module, s)
  if lfNoDecl in s.loc.flags: return
  if sfThreadVar in s.flags: 
    declareThreadVar(p.module, s, sfImportc in s.flags)
  else: 
    if sfImportc in s.flags: app(p.module.s[cfsVars], "extern ")
    app(p.module.s[cfsVars], getTypeDesc(p.module, s.loc.t))
    if sfRegister in s.flags: app(p.module.s[cfsVars], " register")
    if sfVolatile in s.flags: app(p.module.s[cfsVars], " volatile")
    appf(p.module.s[cfsVars], " $1;$n", [s.loc.r])
  if p.module.module.options * {optStackTrace, optEndb} ==
                               {optStackTrace, optEndb}: 
    appcg(p.module, p.module.s[cfsDebugInit], 
          "#dbgRegisterGlobal($1, &$2, $3);$n", 
         [cstringLit(p, p.module.s[cfsDebugInit], 
          normalize(s.owner.name.s & '.' & s.name.s)), 
          s.loc.r, genTypeInfo(p.module, s.typ)])
  
proc assignParam(p: BProc, s: PSym) = 
  assert(s.loc.r != nil)
  if sfAddrTaken in s.flags and gCmd == cmdCompileToLLVM: allocParam(p, s)
  localDebugInfo(p, s)

proc fillProcLoc(sym: PSym) = 
  if sym.loc.k == locNone: 
    fillLoc(sym.loc, locProc, sym.typ, mangleName(sym), OnStack)
  
proc getLabel(p: BProc): TLabel = 
  inc(p.labels)
  result = con("LA", toRope(p.labels))

proc fixLabel(p: BProc, labl: TLabel) = 
  appf(p.s[cpsStmts], "$1: ;$n", [labl])

proc genVarPrototype(m: BModule, sym: PSym)
proc genConstPrototype(m: BModule, sym: PSym)
proc genProc(m: BModule, prc: PSym)
proc genStmts(p: BProc, t: PNode)
proc genProcPrototype(m: BModule, sym: PSym)

include "ccgexprs.nim", "ccgstmts.nim"

# ----------------------------- dynamic library handling -----------------
# We don't finalize dynamic libs as this does the OS for us.

proc libCandidates(s: string, dest: var TStringSeq) = 
  var le = strutils.find(s, '(')
  var ri = strutils.find(s, ')', le+1)
  if le >= 0 and ri > le: 
    var prefix = substr(s, 0, le - 1)
    var suffix = substr(s, ri + 1)
    for middle in split(substr(s, le + 1, ri - 1), '|'):
      libCandidates(prefix & middle & suffix, dest)
  else: 
    add(dest, s)

proc loadDynamicLib(m: BModule, lib: PLib) = 
  assert(lib != nil)
  if not lib.generated: 
    lib.generated = true
    var tmp = getGlobalTempName()
    assert(lib.name == nil)
    lib.name = tmp # BUGFIX: cgsym has awful side-effects
    appf(m.s[cfsVars], "static void* $1;$n", [tmp])
    if lib.path.kind in {nkStrLit..nkTripleStrLit}:
      var s: TStringSeq = @[]
      libCandidates(lib.path.strVal, s)
      var loadlib: PRope = nil
      for i in countup(0, high(s)): 
        inc(m.labels)
        if i > 0: app(loadlib, "||")
        appcg(m, loadlib, "($1 = #nimLoadLibrary((#NimStringDesc*) &$2))$n", 
              [tmp, getStrLit(m, s[i])])
      appcg(m, m.s[cfsDynLibInit], 
            "if (!($1)) #nimLoadLibraryError((#NimStringDesc*) &$2);$n", 
            [loadlib, getStrLit(m, lib.path.strVal)]) 
    else:
      var p = newProc(nil, m)
      var dest: TLoc
      initLocExpr(p, lib.path, dest)
      app(m.s[cfsVars], p.s[cpsLocals])
      app(m.s[cfsDynLibInit], p.s[cpsInit])
      app(m.s[cfsDynLibInit], p.s[cpsStmts])
      appcg(m, m.s[cfsDynLibInit], 
           "if (!($1 = #nimLoadLibrary($2))) #nimLoadLibraryError($2);$n", 
           [tmp, rdLoc(dest)])
      
  if lib.name == nil: InternalError("loadDynamicLib")
  
proc mangleDynLibProc(sym: PSym): PRope =
  if sfCompilerProc in sym.flags: 
    # NOTE: sym.loc.r is the external name!
    result = toRope(sym.name.s)
  else:
    result = ropef("Dl_$1", [toRope(sym.id)])
  
proc SymInDynamicLib(m: BModule, sym: PSym) = 
  var lib = sym.annex
  var extname = sym.loc.r
  loadDynamicLib(m, lib)
  #discard cgsym(m, "nimGetProcAddr")
  if gCmd == cmdCompileToLLVM: incl(sym.loc.flags, lfIndirect)
  var tmp = mangleDynLibProc(sym)
  sym.loc.r = tmp             # from now on we only need the internal name
  sym.typ.sym = nil           # generate a new name
  inc(m.labels, 2)
  appcg(m, m.s[cfsDynLibInit], 
      "$1 = ($2) #nimGetProcAddr($3, $4);$n", 
      [tmp, getTypeDesc(m, sym.typ), 
      lib.name, cstringLit(m, m.s[cfsDynLibInit], ropeToStr(extname))])
  appff(m.s[cfsVars], "$2 $1;$n", 
      "$1 = linkonce global $2 zeroinitializer$n", 
      [sym.loc.r, getTypeDesc(m, sym.loc.t)])

proc cgsym(m: BModule, name: string): PRope = 
  var sym = magicsys.getCompilerProc(name)
  if sym != nil: 
    case sym.kind
    of skProc, skMethod, skConverter: genProc(m, sym)
    of skVar, skResult: genVarPrototype(m, sym)
    of skType: discard getTypeDesc(m, sym.typ)
    else: InternalError("cgsym: " & name)
  else:
    # we used to exclude the system module from this check, but for DLL
    # generation support this sloppyness leads to hard to detect bugs, so
    # we're picky here for the system module too:
    rawMessage(errSystemNeeds, name)
  result = sym.loc.r
  
proc generateHeaders(m: BModule) = 
  app(m.s[cfsHeaders], "#include \"nimbase.h\"" & tnl & tnl)
  var it = PStrEntry(m.headerFiles.head)
  while it != nil: 
    if it.data[0] notin {'\"', '<'}: 
      appf(m.s[cfsHeaders], "#include \"$1\"$n", [toRope(it.data)])
    else: 
      appf(m.s[cfsHeaders], "#include $1$n", [toRope(it.data)])
    it = PStrEntry(it.Next)

proc getFrameDecl(p: BProc) = 
  var slots: PRope
  if p.frameLen > 0: 
    discard cgsym(p.module, "TVarSlot")
    slots = ropeff("  TVarSlot s[$1];$n", ", [$1 x %TVarSlot]", 
                   [toRope(p.frameLen)])
  else: 
    slots = nil
  appff(p.s[cpsLocals], "volatile struct {TFrame* prev;" &
      "NCSTRING procname;NI line;NCSTRING filename;" & 
      "NI len;$n$1} F;$n", 
      "%TF = type {%TFrame*, i8*, %NI, %NI$1}$n" & 
      "%F = alloca %TF$n", [slots])
  inc(p.labels)
  prepend(p.s[cpsInit], ropeff("F.len = $1;$n", 
      "%LOC$2 = getelementptr %TF %F, %NI 4$n" &
      "store %NI $1, %NI* %LOC$2$n", [toRope(p.frameLen), toRope(p.labels)]))

proc retIsNotVoid(s: PSym): bool = 
  result = (s.typ.sons[0] != nil) and not isInvalidReturnType(s.typ.sons[0])

proc initFrame(p: BProc, procname, filename: PRope): PRope = 
  result = ropecg(p.module, 
    "F.procname = $1;$n" &
    "F.filename = $2;$n" & 
    "F.line = 0;$n" & 
    "#pushFrame((TFrame*)&F);$n", [procname, filename])

proc deinitFrame(p: BProc): PRope =
  result = ropecg(p.module, "#popFrame();$n")

proc genProcAux(m: BModule, prc: PSym) =
  var p = newProc(prc, m)
  var header = genProcHeader(m, prc)
  if gCmd != cmdCompileToLLVM and lfExportLib in prc.loc.flags: 
    header = con("N_LIB_EXPORT ", header)
  var returnStmt: PRope = nil
  assert(prc.ast != nil)
  if sfPure notin prc.flags and prc.typ.sons[0] != nil:
    var res = prc.ast.sons[resultPos].sym # get result symbol
    if not isInvalidReturnType(prc.typ.sons[0]): 
      # declare the result symbol:
      assignLocalVar(p, res)
      assert(res.loc.r != nil)
      returnStmt = ropeff("return $1;$n", "ret $1$n", [rdLoc(res.loc)])
      initVariable(p, res)
    else: 
      fillResult(res)
      assignParam(p, res)
      if skipTypes(res.typ, abstractInst).kind == tyArray: 
        incl(res.loc.flags, lfIndirect)
        res.loc.s = OnUnknown
  for i in countup(1, sonsLen(prc.typ.n) - 1): 
    var param = prc.typ.n.sons[i].sym
    assignParam(p, param)
  genStmts(p, prc.ast.sons[codePos]) # modifies p.locals, p.init, etc.
  var generatedProc: PRope
  if sfPure in prc.flags: 
    generatedProc = ropeff("$1 {$n$2$3$4}$n", "define $1 {$n$2$3$4}$n",
        [header, p.s[cpsLocals], p.s[cpsInit], p.s[cpsStmts]])
  else: 
    generatedProc = ropeff("$1 {$n", "define $1 {$n", [header])
    if optStackTrace in prc.options: 
      getFrameDecl(p)
      app(generatedProc, p.s[cpsLocals])
      var procname = CStringLit(p, generatedProc, prc.name.s)
      var filename = CStringLit(p, generatedProc, toFilename(prc.info))
      app(generatedProc, initFrame(p, procname, filename))
    else: 
      app(generatedProc, p.s[cpsLocals])
    if (optProfiler in prc.options) and (gCmd != cmdCompileToLLVM): 
      if gProcProfile >= 64 * 1024: 
        InternalError(prc.info, "too many procedures for profiling")
      discard cgsym(m, "profileData")
      app(p.s[cpsLocals], "ticks NIM_profilingStart;" & tnl)
      if prc.loc.a < 0: 
        appf(m.s[cfsDebugInit], "profileData[$1].procname = $2;$n", [
            toRope(gProcProfile), 
            makeCString(prc.name.s)])
        prc.loc.a = gProcProfile
        inc(gProcProfile)
      prepend(p.s[cpsInit], toRope("NIM_profilingStart = getticks();" & tnl))
    app(generatedProc, p.s[cpsInit])
    app(generatedProc, p.s[cpsStmts])
    if p.beforeRetNeeded: app(generatedProc, "BeforeRet: ;" & tnl)
    if optStackTrace in prc.options: app(generatedProc, deinitFrame(p))
    if (optProfiler in prc.options) and (gCmd != cmdCompileToLLVM): 
      appf(generatedProc, 
        "profileData[$1].total += elapsed(getticks(), NIM_profilingStart);$n", 
        [toRope(prc.loc.a)])
    app(generatedProc, returnStmt)
    app(generatedProc, '}' & tnl)
  app(m.s[cfsProcs], generatedProc)
  
proc genProcPrototype(m: BModule, sym: PSym) = 
  useHeader(m, sym)
  if lfNoDecl in sym.loc.Flags: return 
  if lfDynamicLib in sym.loc.Flags: 
    if sym.owner.id != m.module.id and
        not ContainsOrIncl(m.declaredThings, sym.id): 
      appf(m.s[cfsVars], "extern $1 $2;$n", 
           [getTypeDesc(m, sym.loc.t), mangleDynLibProc(sym)])
      if gCmd == cmdCompileToLLVM: incl(sym.loc.flags, lfIndirect)
  elif not ContainsOrIncl(m.declaredProtos, sym.id): 
    appf(m.s[cfsProcHeaders], "$1;$n", [genProcHeader(m, sym)])

proc genProcNoForward(m: BModule, prc: PSym) = 
  fillProcLoc(prc)
  useHeader(m, prc)
  if lfImportCompilerProc in prc.loc.flags:
    # dependency to a compilerproc:
    discard cgsym(m, prc.name.s)
    return
  genProcPrototype(m, prc)
  if lfNoDecl in prc.loc.Flags: nil
  elif prc.typ.callConv == ccInline:
    # We add inline procs to the calling module to enable C based inlining.
    # This also means that a check with ``gGeneratedSyms`` is wrong, we need
    # a check for ``m.declaredThings``.
    if not ContainsOrIncl(m.declaredThings, prc.id): genProcAux(m, prc)
  elif lfDynamicLib in prc.loc.flags:
    if not ContainsOrIncl(gGeneratedSyms, prc.id): 
      SymInDynamicLib(findPendingModule(m, prc), prc)
  elif sfImportc notin prc.flags: 
    if not ContainsOrIncl(gGeneratedSyms, prc.id): 
      genProcAux(findPendingModule(m, prc), prc)
  
proc genProc(m: BModule, prc: PSym) = 
  if sfBorrow in prc.flags: return 
  fillProcLoc(prc)
  if {sfForward, sfFromGeneric} * prc.flags != {}: addForwardedProc(m, prc)
  else: genProcNoForward(m, prc)
  
proc genVarPrototype(m: BModule, sym: PSym) = 
  assert(sfGlobal in sym.flags)
  useHeader(m, sym)
  fillLoc(sym.loc, locGlobalVar, sym.typ, mangleName(sym), OnHeap)
  if (lfNoDecl in sym.loc.Flags) or ContainsOrIncl(m.declaredThings, sym.id): 
    return 
  if sym.owner.id != m.module.id: 
    # else we already have the symbol generated!
    assert(sym.loc.r != nil)
    if sfThreadVar in sym.flags: 
      declareThreadVar(m, sym, true)
    else:
      app(m.s[cfsVars], "extern ")
      app(m.s[cfsVars], getTypeDesc(m, sym.loc.t))
      if sfRegister in sym.flags: app(m.s[cfsVars], " register")
      if sfVolatile in sym.flags: app(m.s[cfsVars], " volatile")
      appf(m.s[cfsVars], " $1;$n", [sym.loc.r])

proc genConstPrototype(m: BModule, sym: PSym) = 
  useHeader(m, sym)
  if sym.loc.k == locNone: 
    fillLoc(sym.loc, locData, sym.typ, mangleName(sym), OnUnknown)
  if (lfNoDecl in sym.loc.Flags) or
      ContainsOrIncl(m.declaredThings, sym.id): 
    return 
  if sym.owner.id != m.module.id: 
    # else we already have the symbol generated!
    assert(sym.loc.r != nil)
    appff(m.s[cfsData], "extern NIM_CONST $1 $2;$n", 
          "$1 = linkonce constant $2 zeroinitializer", 
          [getTypeDesc(m, sym.loc.t), sym.loc.r])

proc getFileHeader(cfilenoext: string): PRope = 
  if optCompileOnly in gGlobalOptions: 
    result = ropeff("/* Generated by Nimrod Compiler v$1 */$n" &
        "/*   (c) 2011 Andreas Rumpf */$n", 
        "; Generated by Nimrod Compiler v$1$n" &
        ";   (c) 2011 Andreas Rumpf$n", [toRope(versionAsString)])
  else: 
    result = ropeff("/* Generated by Nimrod Compiler v$1 */$n" &
        "/*   (c) 2011 Andreas Rumpf */$n" & 
        "/* Compiled for: $2, $3, $4 */$n" &
        "/* Command for C compiler:$n   $5 */$n", 
        "; Generated by Nimrod Compiler v$1$n" &
        ";   (c) 2011 Andreas Rumpf$n" & "; Compiled for: $2, $3, $4$n" &
        "; Command for LLVM compiler:$n   $5$n", [toRope(versionAsString), 
        toRope(platform.OS[targetOS].name), 
        toRope(platform.CPU[targetCPU].name), 
        toRope(extccomp.CC[extccomp.ccompiler].name), 
        toRope(getCompileCFileCmd(cfilenoext))])
  case platform.CPU[targetCPU].intSize
  of 16: 
    appff(result, 
          "$ntypedef short int NI;$n" & "typedef unsigned short int NU;$n", 
          "$n%NI = type i16$n", [])
  of 32: 
    appff(result, 
          "$ntypedef long int NI;$n" & "typedef unsigned long int NU;$n", 
          "$n%NI = type i32$n", [])
  of 64: 
    appff(result, "$ntypedef long long int NI;$n" &
        "typedef unsigned long long int NU;$n", "$n%NI = type i64$n", [])
  else: 
    nil

proc genMainProc(m: BModule) = 
  const 
    CommonMainBody = 
        "  nim__datInit();$n" &
        "  systemInit();$n" & 
        "$1" & 
        "$2"
    PosixNimMain = 
        "int cmdCount;$n" & 
        "char** cmdLine;$n" & 
        "char** gEnv;$n" &
        "N_CDECL(void, NimMain)(void) {$n" &
        CommonMainBody & "}$n"
    PosixCMain = "int main(int argc, char** args, char** env) {$n" &
        "  cmdLine = args;$n" & "  cmdCount = argc;$n" & "  gEnv = env;$n" &
        "  NimMain();$n" & "  return 0;$n" & "}$n"
    WinNimMain = "N_CDECL(void, NimMain)(void) {$n" &
        CommonMainBody & "}$n"
    WinCMain = "N_STDCALL(int, WinMain)(HINSTANCE hCurInstance, $n" &
        "                        HINSTANCE hPrevInstance, $n" &
        "                        LPSTR lpCmdLine, int nCmdShow) {$n" &
        "  NimMain();$n" & "  return 0;$n" & "}$n"
    WinNimDllMain = "N_LIB_EXPORT N_CDECL(void, NimMain)(void) {$n" &
        CommonMainBody & "}$n"
    WinCDllMain = 
        "BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, $n" &
        "                    LPVOID lpvReserved) {$n" & "  NimMain();$n" &
        "  return 1;$n" & "}$n"
    PosixNimDllMain = WinNimDllMain
    PosixCDllMain = 
        "void NIM_POSIX_INIT NimMainInit(void) {$n" &
        "  NimMain();$n}$n"
  var nimMain, otherMain: TFormatStr
  if platform.targetOS == osWindows and
      gGlobalOptions * {optGenGuiApp, optGenDynLib} != {}: 
    if optGenGuiApp in gGlobalOptions: 
      nimMain = WinNimMain
      otherMain = WinCMain
    else: 
      nimMain = WinNimDllMain
      otherMain = WinCDllMain
    discard lists.IncludeStr(m.headerFiles, "<windows.h>")
  elif optGenDynLib in gGlobalOptions:
    nimMain = posixNimDllMain
    otherMain = posixCDllMain
  else: 
    nimMain = PosixNimMain
    otherMain = PosixCMain
  if gBreakpoints != nil: discard cgsym(m, "dbgRegisterBreakpoint")
  inc(m.labels)
  appcg(m, m.s[cfsProcs], nimMain, [
        gBreakpoints, mainModInit, toRope(m.labels)])
  if not (optNoMain in gGlobalOptions): 
    appcg(m, m.s[cfsProcs], otherMain, [])
  
proc getInitName(m: PSym): PRope = 
  result = ropeff("$1Init", "@$1Init", [toRope(m.name.s)])

proc registerModuleToMain(m: PSym) = 
  var initname = getInitName(m)
  appff(mainModProcs, "N_NOINLINE(void, $1)(void);$n", 
        "declare void $1() noinline$n", [initname])
  if not (sfSystemModule in m.flags): 
    appff(mainModInit, "$1();$n", "call void ()* $1$n", [initname])
  
proc genInitCode(m: BModule) = 
  if optProfiler in m.initProc.options: 
    # This does not really belong here, but there is no good place for this
    # code. I don't want to put this to the proc generation as the
    # ``IncludeStr`` call is quite slow.
    discard lists.IncludeStr(m.headerFiles, "<cycle.h>")
  var initname = getInitName(m.module)
  var prc = ropeff("N_NOINLINE(void, $1)(void) {$n", 
                   "define void $1() noinline {$n", [initname])
  if m.typeNodes > 0: 
    appcg(m, m.s[cfsTypeInit1], "static #TNimNode $1[$2];$n", 
          [m.typeNodesName, toRope(m.typeNodes)])
  if m.nimTypes > 0: 
    appcg(m, m.s[cfsTypeInit1], "static #TNimType $1[$2];$n", 
          [m.nimTypesName, toRope(m.nimTypes)])
  if optStackTrace in m.initProc.options:
    # BUT: the generated init code might depend on a current frame, so
    # declare it nevertheless:
    getFrameDecl(m.initProc)
  if optStackTrace in m.initProc.options and not m.PreventStackTrace: 
    app(prc, m.initProc.s[cpsLocals])
    app(prc, m.s[cfsTypeInit1])
    var procname = CStringLit(m.initProc, prc, m.module.name.s)
    var filename = CStringLit(m.initProc, prc, toFilename(m.module.info))
    app(prc, initFrame(m.initProc, procname, filename))
  else:
    app(prc, m.initProc.s[cpsLocals])
    app(prc, m.s[cfsTypeInit1])
  app(prc, m.s[cfsTypeInit2])
  app(prc, m.s[cfsTypeInit3])
  app(prc, m.s[cfsDebugInit])
  app(prc, m.s[cfsDynLibInit])
  app(prc, m.initProc.s[cpsInit])
  app(prc, m.initProc.s[cpsStmts])
  if optStackTrace in m.initProc.options and not m.PreventStackTrace: 
    app(prc, deinitFrame(m.initProc))
  app(prc, '}' & tnl & tnl)
  app(m.s[cfsProcs], prc)

proc genModule(m: BModule, cfilenoext: string): PRope = 
  result = getFileHeader(cfilenoext)
  generateHeaders(m)
  generateThreadLocalStorage(m)
  for i in countup(low(TCFileSection), cfsProcs): app(result, m.s[i])
  
proc rawNewModule(module: PSym, filename: string): BModule = 
  new(result)
  InitLinkedList(result.headerFiles)
  result.declaredThings = initIntSet()
  result.declaredProtos = initIntSet()
  result.cfilename = filename
  result.filename = filename
  initIdTable(result.typeCache)
  initIdTable(result.forwTypeCache)
  result.module = module
  result.typeInfoMarker = initIntSet()
  result.initProc = newProc(nil, result)
  result.initProc.options = gOptions
  initNodeTable(result.dataCache)
  result.typeStack = @[]
  result.forwardedProcs = @[]
  result.typeNodesName = getTempName()
  result.nimTypesName = getTempName()
  result.PreventStackTrace = sfSystemModule in module.flags

proc newModule(module: PSym, filename: string): BModule = 
  result = rawNewModule(module, filename)
  if (optDeadCodeElim in gGlobalOptions): 
    if (sfDeadCodeElim in module.flags): 
      InternalError("added pending module twice: " & filename)
    addPendingModule(result)

proc registerTypeInfoModule() = 
  const moduleName = "nim__dat"
  var s = NewSym(skModule, getIdent(moduleName), nil)
  gNimDat = rawNewModule(s, joinPath(options.projectPath, moduleName) & ".nim")
  gNimDat.PreventStackTrace = true
  addPendingModule(gNimDat)
  appff(mainModProcs, "N_NOINLINE(void, $1)(void);$n", 
        "declare void $1() noinline$n", [getInitName(s)])

proc myOpen(module: PSym, filename: string): PPassContext = 
  if gNimDat == nil: registerTypeInfoModule()
  result = newModule(module, filename)

proc myOpenCached(module: PSym, filename: string, 
                  rd: PRodReader): PPassContext = 
  if gNimDat == nil: 
    registerTypeInfoModule()
  var cfile = changeFileExt(completeCFilePath(filename), cExt)
  var cfilenoext = changeFileExt(cfile, "")
  addFileToLink(cfilenoext)
  registerModuleToMain(module)
  # XXX: this cannot be right here, initalization has to be appended during
  # the ``myClose`` call
  result = nil

proc shouldRecompile(code: PRope, cfile, cfilenoext: string): bool = 
  result = true
  if not (optForceFullMake in gGlobalOptions): 
    var objFile = toObjFile(cfilenoext)
    if writeRopeIfNotEqual(code, cfile): return 
    if ExistsFile(objFile) and os.FileNewer(objFile, cfile): result = false
  else: 
    writeRope(code, cfile)
  
proc myProcess(b: PPassContext, n: PNode): PNode = 
  result = n
  if b == nil or passes.skipCodegen(n): return
  var m = BModule(b)
  m.initProc.options = gOptions
  genStmts(m.initProc, n)

proc finishModule(m: BModule) = 
  var i = 0
  while i <= high(m.forwardedProcs): 
    # Note: ``genProc`` may add to ``m.forwardedProcs``, so we cannot use
    # a ``for`` loop here
    var prc = m.forwardedProcs[i]
    if sfForward in prc.flags: 
      InternalError(prc.info, "still forwarded: " & prc.name.s)
    genProcNoForward(m, prc)
    inc(i)
  assert(gForwardedProcsCounter >= i)
  dec(gForwardedProcsCounter, i)
  setlen(m.forwardedProcs, 0)

proc writeModule(m: BModule) = 
  # generate code for the init statements of the module:
  genInitCode(m)
  finishTypeDescriptions(m)
  var cfile = completeCFilePath(m.cfilename)
  var cfilenoext = changeFileExt(cfile, "")
  if sfMainModule in m.module.flags: 
    # generate main file:
    app(m.s[cfsProcHeaders], mainModProcs)
    GenerateThreadVarsSize(m)
  var code = genModule(m, cfilenoext)
  
  when hasTinyCBackend:
    if gCmd == cmdRun:
      tccgen.compileCCode(ropeToStr(code))
      return
  
  if shouldRecompile(code, changeFileExt(cfile, cExt), cfilenoext): 
    addFileToCompile(cfilenoext)
  addFileToLink(cfilenoext)

proc myClose(b: PPassContext, n: PNode): PNode = 
  result = n
  if b == nil or passes.skipCodegen(n): return 
  var m = BModule(b)
  if n != nil: 
    m.initProc.options = gOptions
    genStmts(m.initProc, n)
  registerModuleToMain(m.module)
  if not (optDeadCodeElim in gGlobalOptions) and
      not (sfDeadCodeElim in m.module.flags): 
    finishModule(m)
  if sfMainModule in m.module.flags: 
    var disp = generateMethodDispatchers()
    for i in 0..sonsLen(disp)-1: genProcAux(gNimDat, disp.sons[i].sym)
    genMainProc(m) 
    # we need to process the transitive closure because recursive module
    # deps are allowed (and the system module is processed in the wrong
    # order anyway)
    while gForwardedProcsCounter > 0: 
      for i in countup(0, high(gPendingModules)): 
        finishModule(gPendingModules[i])
    for i in countup(0, high(gPendingModules)): writeModule(gPendingModules[i])
    setlen(gPendingModules, 0)
  if not (optDeadCodeElim in gGlobalOptions) and
      not (sfDeadCodeElim in m.module.flags): 
    writeModule(m)
  if sfMainModule in m.module.flags: writeMapping(gMapping)
  
proc cgenPass(): TPass = 
  initPass(result)
  result.open = myOpen
  result.openCached = myOpenCached
  result.process = myProcess
  result.close = myClose

InitIiTable(gToTypeInfoId)
gGeneratedSyms = initIntSet()
