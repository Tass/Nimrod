#
#
#           The Nimrod Compiler
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module is responsible for loading of rod files.
#
#  Reading and writing binary files are really hard to debug. Therefore we use
#  a special text format. ROD-files are more efficient to process because
#  symbols are only loaded on demand.
#  It consists of:
#
#  - a header:
#    NIM:$fileversion\n
#  - the module's id (even if the module changed, its ID will not!):
#    ID:Ax3\n
#  - CRC value of this module:
#    CRC:CRC-val\n
#  - a section containing the compiler options and defines this
#    module has been compiled with:
#    OPTIONS:options\n
#    DEFINES:defines\n
#  - FILES(
#    myfile.inc
#    lib/mymodA
#    )
#  - a include file dependency section:
#    INCLUDES(
#    <fileidx> <CRC of myfile.inc>\n # fileidx is the LINE in the file section!
#    )
#  - a module dependency section:
#    DEPS: <fileidx> <fileidx>\n
#  - an interface section:
#    INTERF(
#    identifier1 id\n # id is the symbol's id
#    identifier2 id\n
#    )
#  - a compiler proc section:
#    COMPILERPROCS(
#    identifier1 id\n # id is the symbol's id    
#    )
#  - an index consisting of (ID, linenumber)-pairs:
#    INDEX(
#    id-diff idx-diff\n
#    id-diff idx-diff\n
#    )
#  - an import index consisting of (ID, moduleID)-pairs:
#    IMPORTS(
#    id-diff moduleID-diff\n
#    id-diff moduleID-diff\n
#    )
#  - a list of all exported type converters because they are needed for correct
#    semantic checking:
#    CONVERTERS:id id\n   # position of the symbol in the DATA section
#  - an AST section that contains the module's AST:
#    INIT(
#    idx\n  # position of the node in the DATA section
#    idx\n
#    )
#  - a data section, where each type, symbol or AST is stored.
#    DATA(
#    type
#    (node)
#    sym
#    )
#
#  We now also do index compression, because an index always needs to be read.
#

import 
  os, options, strutils, nversion, ast, astalgo, msgs, platform, condsyms, 
  ropes, idents, crc, idgen, rodutils

type 
  TReasonForRecompile* = enum 
    rrEmpty,                  # used by moddeps module
    rrNone,                   # no need to recompile
    rrRodDoesNotExist,        # rod file does not exist
    rrRodInvalid,             # rod file is invalid
    rrCrcChange,              # file has been edited since last recompilation
    rrDefines,                # defines have changed
    rrOptions,                # options have changed
    rrInclDeps,               # an include has changed
    rrModDeps                 # a module this module depends on has been changed

const 
  reasonToFrmt*: array[TReasonForRecompile, string] = ["", 
    "no need to recompile: $1", "symbol file for $1 does not exist", 
    "symbol file for $1 has the wrong version", 
    "file edited since last compilation: $1", 
    "list of conditional symbols changed for: $1", 
    "list of options changed for: $1", "an include file edited: $1", 
    "a module $1 depends on has changed"]

type
  TIndex*{.final.} = object   # an index with compression
    lastIdxKey*, lastIdxVal*: int
    tab*: TIITable
    r*: string                # writers use this
    offset*: int              # readers use this
  
  TRodReader* = object of TObject
    pos*: int                 # position; used for parsing
    s*: string                # the whole file in memory; XXX mmap this!
    options*: TOptions
    reason*: TReasonForRecompile
    modDeps*: TStringSeq
    files*: TStringSeq
    dataIdx*: int             # offset of start of data section
    convertersIdx*: int       # offset of start of converters section
    initIdx*, interfIdx*, compilerProcsIdx*, cgenIdx*: int
    filename*: string
    index*, imports*: TIndex
    readerIndex*: int
    line*: int            # only used for debugging, but is always in the code
    moduleID*: int
    syms*: TIdTable           # already processed symbols
  
  PRodReader* = ref TRodReader

const 
  FileVersion* = "1018"       # modify this if the rod-format changes!

var rodCompilerprocs*: TStrTable

proc handleSymbolFile*(module: PSym, filename: string): PRodReader
# global because this is needed by magicsys
proc loadInitSection*(r: PRodReader): PNode
proc loadStub*(s: PSym)

# implementation

var gTypeTable: TIdTable

proc rrGetSym(r: PRodReader, id: int, info: TLineInfo): PSym
  # `info` is only used for debugging purposes
proc rrGetType(r: PRodReader, id: int, info: TLineInfo): PType

proc decodeLineInfo(r: PRodReader, info: var TLineInfo) = 
  if r.s[r.pos] == '?': 
    inc(r.pos)
    if r.s[r.pos] == ',': info.col = -1'i16
    else: info.col = int16(decodeVInt(r.s, r.pos))
    if r.s[r.pos] == ',': 
      inc(r.pos)
      if r.s[r.pos] == ',': info.line = -1'i16
      else: info.line = int16(decodeVInt(r.s, r.pos))
      if r.s[r.pos] == ',': 
        inc(r.pos)
        info = newLineInfo(r.files[decodeVInt(r.s, r.pos)], info.line, info.col)

proc decodeNode(r: PRodReader, fInfo: TLineInfo): PNode = 
  result = nil
  if r.s[r.pos] == '(': 
    inc(r.pos)
    if r.s[r.pos] == ')': 
      inc(r.pos)
      return                  # nil node
    result = newNodeI(TNodeKind(decodeVInt(r.s, r.pos)), fInfo)
    decodeLineInfo(r, result.info)
    if r.s[r.pos] == '$': 
      inc(r.pos)
      result.flags = cast[TNodeFlags](int32(decodeVInt(r.s, r.pos)))
    if r.s[r.pos] == '^': 
      inc(r.pos)
      var id = decodeVInt(r.s, r.pos)
      result.typ = rrGetType(r, id, result.info)
    case result.kind
    of nkCharLit..nkInt64Lit: 
      if r.s[r.pos] == '!': 
        inc(r.pos)
        result.intVal = decodeVBiggestInt(r.s, r.pos)
    of nkFloatLit..nkFloat64Lit: 
      if r.s[r.pos] == '!': 
        inc(r.pos)
        var fl = decodeStr(r.s, r.pos)
        result.floatVal = parseFloat(fl)
    of nkStrLit..nkTripleStrLit: 
      if r.s[r.pos] == '!': 
        inc(r.pos)
        result.strVal = decodeStr(r.s, r.pos)
      else: 
        result.strVal = ""    # BUGFIX
    of nkIdent: 
      if r.s[r.pos] == '!': 
        inc(r.pos)
        var fl = decodeStr(r.s, r.pos)
        result.ident = getIdent(fl)
      else: 
        internalError(result.info, "decodeNode: nkIdent")
    of nkSym: 
      if r.s[r.pos] == '!': 
        inc(r.pos)
        var id = decodeVInt(r.s, r.pos)
        result.sym = rrGetSym(r, id, result.info)
      else: 
        internalError(result.info, "decodeNode: nkSym")
    else: 
      while r.s[r.pos] != ')': addSon(result, decodeNode(r, result.info))
    if r.s[r.pos] == ')': inc(r.pos)
    else: internalError(result.info, "decodeNode")
  else: 
    InternalError(result.info, "decodeNode " & r.s[r.pos])
  
proc decodeLoc(r: PRodReader, loc: var TLoc, info: TLineInfo) = 
  if r.s[r.pos] == '<': 
    inc(r.pos)
    if r.s[r.pos] in {'0'..'9', 'a'..'z', 'A'..'Z'}: 
      loc.k = TLocKind(decodeVInt(r.s, r.pos))
    else: 
      loc.k = low(loc.k)
    if r.s[r.pos] == '*': 
      inc(r.pos)
      loc.s = TStorageLoc(decodeVInt(r.s, r.pos))
    else: 
      loc.s = low(loc.s)
    if r.s[r.pos] == '$': 
      inc(r.pos)
      loc.flags = cast[TLocFlags](int32(decodeVInt(r.s, r.pos)))
    else: 
      loc.flags = {}
    if r.s[r.pos] == '^': 
      inc(r.pos)
      loc.t = rrGetType(r, decodeVInt(r.s, r.pos), info)
    else: 
      loc.t = nil
    if r.s[r.pos] == '!': 
      inc(r.pos)
      loc.r = toRope(decodeStr(r.s, r.pos))
    else: 
      loc.r = nil
    if r.s[r.pos] == '?': 
      inc(r.pos)
      loc.a = decodeVInt(r.s, r.pos)
    else: 
      loc.a = 0
    if r.s[r.pos] == '>': inc(r.pos)
    else: InternalError(info, "decodeLoc " & r.s[r.pos])
  
proc decodeType(r: PRodReader, info: TLineInfo): PType = 
  result = nil
  if r.s[r.pos] == '[': 
    inc(r.pos)
    if r.s[r.pos] == ']': 
      inc(r.pos)
      return                  # nil type
  new(result)
  result.kind = TTypeKind(decodeVInt(r.s, r.pos))
  if r.s[r.pos] == '+': 
    inc(r.pos)
    result.id = decodeVInt(r.s, r.pos)
    setId(result.id)
    if debugIds: registerID(result)
  else: 
    InternalError(info, "decodeType: no id")
  # here this also avoids endless recursion for recursive type
  IdTablePut(gTypeTable, result, result) 
  if r.s[r.pos] == '(': result.n = decodeNode(r, UnknownLineInfo())
  if r.s[r.pos] == '$': 
    inc(r.pos)
    result.flags = cast[TTypeFlags](int32(decodeVInt(r.s, r.pos)))
  if r.s[r.pos] == '?': 
    inc(r.pos)
    result.callConv = TCallingConvention(decodeVInt(r.s, r.pos))
  if r.s[r.pos] == '*': 
    inc(r.pos)
    result.owner = rrGetSym(r, decodeVInt(r.s, r.pos), info)
  if r.s[r.pos] == '&': 
    inc(r.pos)
    result.sym = rrGetSym(r, decodeVInt(r.s, r.pos), info)
  if r.s[r.pos] == '/': 
    inc(r.pos)
    result.size = decodeVInt(r.s, r.pos)
  else: 
    result.size = - 1
  if r.s[r.pos] == '=': 
    inc(r.pos)
    result.align = decodeVInt(r.s, r.pos)
  else: 
    result.align = 2
  if r.s[r.pos] == '@': 
    inc(r.pos)
    result.containerID = decodeVInt(r.s, r.pos)
  decodeLoc(r, result.loc, info)
  while r.s[r.pos] == '^': 
    inc(r.pos)
    if r.s[r.pos] == '(': 
      inc(r.pos)
      if r.s[r.pos] == ')': inc(r.pos)
      else: InternalError(info, "decodeType ^(" & r.s[r.pos])
      addSon(result, nil)
    else: 
      var d = decodeVInt(r.s, r.pos)
      addSon(result, rrGetType(r, d, info))

proc decodeLib(r: PRodReader, info: TLineInfo): PLib = 
  result = nil
  if r.s[r.pos] == '|': 
    new(result)
    inc(r.pos)
    result.kind = TLibKind(decodeVInt(r.s, r.pos))
    if r.s[r.pos] != '|': InternalError("decodeLib: 1")
    inc(r.pos)
    result.name = toRope(decodeStr(r.s, r.pos))
    if r.s[r.pos] != '|': InternalError("decodeLib: 2")
    inc(r.pos)
    result.path = decodeNode(r, info)

proc decodeSym(r: PRodReader, info: TLineInfo): PSym = 
  var 
    id: int
    ident: PIdent
  result = nil
  if r.s[r.pos] == '{': 
    inc(r.pos)
    if r.s[r.pos] == '}': 
      inc(r.pos)
      return                  # nil sym
  var k = TSymKind(decodeVInt(r.s, r.pos))
  if r.s[r.pos] == '+': 
    inc(r.pos)
    id = decodeVInt(r.s, r.pos)
    setId(id)
  else: 
    InternalError(info, "decodeSym: no id")
  if r.s[r.pos] == '&': 
    inc(r.pos)
    ident = getIdent(decodeStr(r.s, r.pos))
  else: 
    InternalError(info, "decodeSym: no ident")
  result = PSym(IdTableGet(r.syms, id))
  if result == nil: 
    new(result)
    result.id = id
    IdTablePut(r.syms, result, result)
    if debugIds: registerID(result)
  elif (result.id != id): 
    InternalError(info, "decodeSym: wrong id")
  result.kind = k
  result.name = ident         # read the rest of the symbol description:
  if r.s[r.pos] == '^': 
    inc(r.pos)
    result.typ = rrGetType(r, decodeVInt(r.s, r.pos), info)
  decodeLineInfo(r, result.info)
  if r.s[r.pos] == '*': 
    inc(r.pos)
    result.owner = rrGetSym(r, decodeVInt(r.s, r.pos), result.info)
  if r.s[r.pos] == '$': 
    inc(r.pos)
    result.flags = cast[TSymFlags](int32(decodeVInt(r.s, r.pos)))
  if r.s[r.pos] == '@': 
    inc(r.pos)
    result.magic = TMagic(decodeVInt(r.s, r.pos))
  if r.s[r.pos] == '(': result.ast = decodeNode(r, result.info)
  if r.s[r.pos] == '!': 
    inc(r.pos)
    result.options = cast[TOptions](int32(decodeVInt(r.s, r.pos)))
  else: 
    result.options = r.options
  if r.s[r.pos] == '%': 
    inc(r.pos)
    result.position = decodeVInt(r.s, r.pos)
  else: 
    result.position = 0       
    # BUGFIX: this may have been misused as reader index!
  if r.s[r.pos] == '`': 
    inc(r.pos)
    result.offset = decodeVInt(r.s, r.pos)
  else: 
    result.offset = - 1
  decodeLoc(r, result.loc, result.info)
  result.annex = decodeLib(r, info)

proc skipSection(r: PRodReader) = 
  if r.s[r.pos] == ':': 
    while r.s[r.pos] > '\x0A': inc(r.pos)
  elif r.s[r.pos] == '(': 
    var c = 0                 # count () pairs
    inc(r.pos)
    while true: 
      case r.s[r.pos]
      of '\x0A': inc(r.line)
      of '(': inc(c)
      of ')': 
        if c == 0: 
          inc(r.pos)
          break 
        elif c > 0: 
          dec(c)
      of '\0': break          # end of file
      else: nil
      inc(r.pos)
  else: 
    InternalError("skipSection " & $r.line)
  
proc rdWord(r: PRodReader): string = 
  result = ""
  while r.s[r.pos] in {'A'..'Z', '_', 'a'..'z', '0'..'9'}: 
    add(result, r.s[r.pos])
    inc(r.pos)

proc newStub(r: PRodReader, name: string, id: int): PSym = 
  new(result)
  result.kind = skStub
  result.id = id
  result.name = getIdent(name)
  result.position = r.readerIndex
  setID(id)                   #MessageOut(result.name.s);
  if debugIds: registerID(result)
  
proc processInterf(r: PRodReader, module: PSym) = 
  if r.interfIdx == 0: InternalError("processInterf")
  r.pos = r.interfIdx
  while (r.s[r.pos] > '\x0A') and (r.s[r.pos] != ')'): 
    var w = decodeStr(r.s, r.pos)
    inc(r.pos)
    var key = decodeVInt(r.s, r.pos)
    inc(r.pos)                # #10
    var s = newStub(r, w, key)
    s.owner = module
    StrTableAdd(module.tab, s)
    IdTablePut(r.syms, s, s)

proc processCompilerProcs(r: PRodReader, module: PSym) = 
  if r.compilerProcsIdx == 0: InternalError("processCompilerProcs")
  r.pos = r.compilerProcsIdx
  while (r.s[r.pos] > '\x0A') and (r.s[r.pos] != ')'): 
    var w = decodeStr(r.s, r.pos)
    inc(r.pos)
    var key = decodeVInt(r.s, r.pos)
    inc(r.pos)                # #10
    var s = PSym(IdTableGet(r.syms, key))
    if s == nil: 
      s = newStub(r, w, key)
      s.owner = module
      IdTablePut(r.syms, s, s)
    StrTableAdd(rodCompilerProcs, s)

proc processIndex(r: PRodReader, idx: var TIndex) = 
  var key, val, tmp: int
  inc(r.pos, 2)               # skip "(\10"
  inc(r.line)
  while (r.s[r.pos] > '\x0A') and (r.s[r.pos] != ')'): 
    tmp = decodeVInt(r.s, r.pos)
    if r.s[r.pos] == ' ': 
      inc(r.pos)
      key = idx.lastIdxKey + tmp
      val = decodeVInt(r.s, r.pos) + idx.lastIdxVal
    else: 
      key = idx.lastIdxKey + 1
      val = tmp + idx.lastIdxVal
    IITablePut(idx.tab, key, val)
    idx.lastIdxKey = key
    idx.lastIdxVal = val
    setID(key)                # ensure that this id will not be used
    if r.s[r.pos] == '\x0A': 
      inc(r.pos)
      inc(r.line)
  if r.s[r.pos] == ')': inc(r.pos)
  
proc processRodFile(r: PRodReader, crc: TCrc32) = 
  var 
    w: string
    d, L, inclCrc: int
  while r.s[r.pos] != '\0': 
    var section = rdWord(r)
    if r.reason != rrNone: 
      break                   # no need to process this file further
    case section 
    of "CRC": 
      inc(r.pos)              # skip ':'
      if int(crc) != decodeVInt(r.s, r.pos): r.reason = rrCrcChange
    of "ID": 
      inc(r.pos)              # skip ':'
      r.moduleID = decodeVInt(r.s, r.pos)
      setID(r.moduleID)
    of "OPTIONS": 
      inc(r.pos)              # skip ':'
      r.options = cast[TOptions](int32(decodeVInt(r.s, r.pos)))
      if options.gOptions != r.options: r.reason = rrOptions
    of "DEFINES": 
      inc(r.pos)              # skip ':'
      d = 0
      while r.s[r.pos] > '\x0A': 
        w = decodeStr(r.s, r.pos)
        inc(d)
        if not condsyms.isDefined(getIdent(w)): 
          r.reason = rrDefines #MessageOut('not defined, but should: ' + w);
        if r.s[r.pos] == ' ': inc(r.pos)
      if (d != countDefinedSymbols()): r.reason = rrDefines
    of "FILES": 
      inc(r.pos, 2)           # skip "(\10"
      inc(r.line)
      L = 0
      while r.s[r.pos] > '\x0A' and r.s[r.pos] != ')': 
        setlen(r.files, L + 1)
        r.files[L] = decodeStr(r.s, r.pos)
        inc(r.pos)            # skip #10
        inc(r.line)
        inc(L)
      if r.s[r.pos] == ')': inc(r.pos)
    of "INCLUDES": 
      inc(r.pos, 2)           # skip "(\10"
      inc(r.line)
      while r.s[r.pos] > '\x0A' and r.s[r.pos] != ')': 
        w = r.files[decodeVInt(r.s, r.pos)]
        inc(r.pos)            # skip ' '
        inclCrc = decodeVInt(r.s, r.pos)
        if r.reason == rrNone: 
          if not ExistsFile(w) or (inclCrc != int(crcFromFile(w))): 
            r.reason = rrInclDeps
        if r.s[r.pos] == '\x0A': 
          inc(r.pos)
          inc(r.line)
      if r.s[r.pos] == ')': inc(r.pos)
    of "DEPS": 
      inc(r.pos)              # skip ':'
      L = 0
      while r.s[r.pos] > '\x0A': 
        setlen(r.modDeps, L + 1)
        r.modDeps[L] = r.files[decodeVInt(r.s, r.pos)]
        inc(L)
        if r.s[r.pos] == ' ': inc(r.pos)
    of "INTERF": 
      r.interfIdx = r.pos + 2
      skipSection(r)
    of "COMPILERPROCS": 
      r.compilerProcsIdx = r.pos + 2
      skipSection(r)
    of "INDEX": 
      processIndex(r, r.index)
    of "IMPORTS": 
      processIndex(r, r.imports)
    of "CONVERTERS": 
      r.convertersIdx = r.pos + 1
      skipSection(r)
    of "DATA": 
      r.dataIdx = r.pos + 2 # "(\10"
      # We do not read the DATA section here! We read the needed objects on
      # demand.
      skipSection(r)
    of "INIT": 
      r.initIdx = r.pos + 2   # "(\10"
      skipSection(r)
    of "CGEN": 
      r.cgenIdx = r.pos + 2
      skipSection(r)
    else: 
      MsgWriteln("skipping section: " & $r.pos)
      skipSection(r)
    if r.s[r.pos] == '\x0A': 
      inc(r.pos)
      inc(r.line)

proc newRodReader(modfilename: string, crc: TCrc32, 
                  readerIndex: int): PRodReader = 
  new(result)
  result.files = @[]
  result.modDeps = @[]
  var r = result
  r.reason = rrNone
  r.pos = 0
  r.line = 1
  r.readerIndex = readerIndex
  r.filename = modfilename
  InitIdTable(r.syms)
  r.s = readFile(modfilename)
  if startsWith(r.s, "NIM:"): 
    initIITable(r.index.tab)
    initIITable(r.imports.tab) # looks like a ROD file
    inc(r.pos, 4)
    var version = ""
    while r.s[r.pos] notin {'\0', '\x0A'}:
      add(version, r.s[r.pos])
      inc(r.pos)
    if r.s[r.pos] == '\x0A': inc(r.pos)
    if version == FileVersion: 
      # since ROD files are only for caching, no backwarts compatibility is
      # needed
      processRodFile(r, crc)
    else: 
      result = nil
  else: 
    result = nil
  
proc rrGetType(r: PRodReader, id: int, info: TLineInfo): PType = 
  result = PType(IdTableGet(gTypeTable, id))
  if result == nil: 
    # load the type:
    var oldPos = r.pos
    var d = IITableGet(r.index.tab, id)
    if d == invalidKey: InternalError(info, "rrGetType")
    r.pos = d + r.dataIdx
    result = decodeType(r, info)
    r.pos = oldPos

type 
  TFileModuleRec{.final.} = object 
    filename*: string
    reason*: TReasonForRecompile
    rd*: PRodReader
    crc*: TCrc32

  TFileModuleMap = seq[TFileModuleRec]

var gMods: TFileModuleMap = @[]

proc decodeSymSafePos(rd: PRodReader, offset: int, info: TLineInfo): PSym = 
  # all compiled modules
  if rd.dataIdx == 0: InternalError(info, "dataIdx == 0")
  var oldPos = rd.pos
  rd.pos = offset + rd.dataIdx
  result = decodeSym(rd, info)
  rd.pos = oldPos

proc rrGetSym(r: PRodReader, id: int, info: TLineInfo): PSym = 
  result = PSym(IdTableGet(r.syms, id))
  if result == nil: 
    # load the symbol:
    var d = IITableGet(r.index.tab, id)
    if d == invalidKey: 
      var moduleID = IiTableGet(r.imports.tab, id)
      if moduleID < 0: 
        var x = ""
        encodeVInt(id, x)
        InternalError(info, "missing from both indexes: +" & x) 
      # find the reader with the correct moduleID:
      for i in countup(0, high(gMods)): 
        var rd = gMods[i].rd
        if rd != nil: 
          if rd.moduleID == moduleID: 
            d = IITableGet(rd.index.tab, id)
            if d != invalidKey: 
              result = decodeSymSafePos(rd, d, info)
              break 
            else:
              var x = ""
              encodeVInt(id, x)
              InternalError(info, "rrGetSym: no reader found: +" & x)
          else:
            #if IiTableGet(rd.index.tab, id) <> invalidKey then
            # XXX expensive check!
            #InternalError(info,
            #'id found in other module: +' + ropeToStr(encodeInt(id)))
    else: 
      # own symbol:
      result = decodeSymSafePos(r, d, info)
  if result != nil and result.kind == skStub: loadStub(result)
  
proc loadInitSection(r: PRodReader): PNode = 
  if r.initIdx == 0 or r.dataIdx == 0: InternalError("loadInitSection")
  var oldPos = r.pos
  r.pos = r.initIdx
  result = newNode(nkStmtList)
  while r.s[r.pos] > '\x0A' and r.s[r.pos] != ')': 
    var d = decodeVInt(r.s, r.pos)
    inc(r.pos)                # #10
    var p = r.pos
    r.pos = d + r.dataIdx
    addSon(result, decodeNode(r, UnknownLineInfo()))
    r.pos = p
  r.pos = oldPos

proc loadConverters(r: PRodReader) = 
  # We have to ensure that no exported converter is a stub anymore.
  if r.convertersIdx == 0 or r.dataIdx == 0: 
    InternalError("importConverters")
  r.pos = r.convertersIdx
  while r.s[r.pos] > '\x0A': 
    var d = decodeVInt(r.s, r.pos)
    discard rrGetSym(r, d, UnknownLineInfo())
    if r.s[r.pos] == ' ': inc(r.pos)
  
proc getModuleIdx(filename: string): int = 
  for i in countup(0, high(gMods)): 
    if sameFile(gMods[i].filename, filename): return i
  result = len(gMods)
  setlen(gMods, result + 1)

proc checkDep(filename: string): TReasonForRecompile = 
  var idx = getModuleIdx(filename)
  if gMods[idx].reason != rrEmpty: 
    # reason has already been computed for this module:
    return gMods[idx].reason
  var crc: TCrc32 = crcFromFile(filename)
  gMods[idx].reason = rrNone  # we need to set it here to avoid cycles
  gMods[idx].filename = filename
  gMods[idx].crc = crc
  result = rrNone
  var r: PRodReader = nil
  var rodfile = toGeneratedFile(filename, RodExt)
  if ExistsFile(rodfile): 
    r = newRodReader(rodfile, crc, idx)
    if r == nil: 
      result = rrRodInvalid
    else: 
      result = r.reason
      if result == rrNone: 
        # check modules it depends on
        # NOTE: we need to process the entire module graph so that no ID will
        # be used twice! However, compilation speed does not suffer much from
        # this, since results are cached.
        var res = checkDep(options.libpath / addFileExt("system", nimExt))
        if res != rrNone: result = rrModDeps
        for i in countup(0, high(r.modDeps)): 
          res = checkDep(r.modDeps[i])
          if res != rrNone: 
            result = rrModDeps 
            # we cannot break here, because of side-effects of `checkDep`
  else: 
    result = rrRodDoesNotExist
  if result != rrNone and gVerbosity > 0: 
    MsgWriteln(`%`(reasonToFrmt[result], [filename]))
  if result != rrNone or optForceFullMake in gGlobalOptions: 
    # recompilation is necessary:
    r = nil
  gMods[idx].rd = r
  gMods[idx].reason = result  # now we know better
  
proc handleSymbolFile(module: PSym, filename: string): PRodReader = 
  if optSymbolFiles notin gGlobalOptions: 
    module.id = getID()
    return nil
  idgen.loadMaxIds(options.projectPath / options.projectName)

  discard checkDep(filename)
  var idx = getModuleIdx(filename)
  if gMods[idx].reason == rrEmpty: InternalError("handleSymbolFile")
  result = gMods[idx].rd
  if result != nil: 
    module.id = result.moduleID
    IdTablePut(result.syms, module, module)
    processInterf(result, module)
    processCompilerProcs(result, module)
    loadConverters(result)
  else: 
    module.id = getID()
  
proc GetCRC*(filename: string): TCrc32 = 
  var idx = getModuleIdx(filename)
  result = gMods[idx].crc

proc loadStub(s: PSym) =
  if s.kind != skStub: InternalError("loadStub")
  var rd = gMods[s.position].rd
  var theId = s.id                # used for later check
  var d = IITableGet(rd.index.tab, s.id)
  if d == invalidKey: InternalError("loadStub: invalid key")
  var rs = decodeSymSafePos(rd, d, UnknownLineInfo())
  if rs != s: 
    InternalError(rs.info, "loadStub: wrong symbol")
  elif rs.id != theId: 
    InternalError(rs.info, "loadStub: wrong ID") 
  #MessageOut('loaded stub: ' + s.name.s);
  
InitIdTable(gTypeTable)
InitStrTable(rodCompilerProcs)
