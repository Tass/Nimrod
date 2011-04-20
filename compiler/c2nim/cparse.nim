#
#
#      c2nim - C to Nimrod source converter
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements an Ansi C parser.
## It translates a C source file into a Nimrod AST. Then the renderer can be
## used to convert the AST to its text representation.

# XXX cleanup of declaration handling.

import 
  os, llstream, renderer, clex, idents, strutils, pegs, ast, astalgo, msgs,
  options, strtabs

type 
  TParserFlag = enum
    pfRefs,             ## use "ref" instead of "ptr" for C's typ*
    pfCDecl,            ## annotate procs with cdecl
    pfStdCall,          ## annotate procs with stdcall
    pfSkipInclude,      ## skip all ``#include``
    pfTypePrefixes,     ## all generated types start with 'T' or 'P'
    pfSkipComments      ## do not generate comments
  
  TMacro {.final.} = object
    name: string
    params: int           # number of parameters
    body: seq[ref TToken] # can contain pxMacroParam tokens
  
  TParserOptions {.final.} = object
    flags: set[TParserFlag]
    prefixes, suffixes: seq[string]
    mangleRules: seq[tuple[pattern: TPeg, frmt: string]]
    privateRules: seq[TPeg]
    dynlibSym, header: string
    macros: seq[TMacro]
    toMangle: PStringTable
  PParserOptions* = ref TParserOptions
  
  TParser* {.final.} = object
    lex: TLexer
    tok: ref TToken       # current token
    options: PParserOptions
    backtrack: seq[ref TToken]
    inTypeDef: int
    scopeCounter: int
    hasDeadCodeElimPragma: bool
  
  TReplaceTuple* = array[0..1, string]

proc newParserOptions*(): PParserOptions = 
  new(result)
  result.prefixes = @[]
  result.suffixes = @[]
  result.macros = @[]
  result.mangleRules = @[]
  result.privateRules = @[]
  result.flags = {}
  result.dynlibSym = ""
  result.header = ""
  result.toMangle = newStringTable()

proc setOption*(parserOptions: PParserOptions, key: string, val=""): bool = 
  result = true
  case key
  of "ref": incl(parserOptions.flags, pfRefs)
  of "dynlib": parserOptions.dynlibSym = val
  of "header": parserOptions.header = val
  of "cdecl": incl(parserOptions.flags, pfCdecl)
  of "stdcall": incl(parserOptions.flags, pfStdCall)
  of "prefix": parserOptions.prefixes.add(val)
  of "suffix": parserOptions.suffixes.add(val)
  of "skipinclude": incl(parserOptions.flags, pfSkipInclude)
  of "typeprefixes": incl(parserOptions.flags, pfTypePrefixes)
  of "skipcomments": incl(parserOptions.flags, pfSkipComments)
  else: result = false

proc ParseUnit*(p: var TParser): PNode
proc openParser*(p: var TParser, filename: string, inputStream: PLLStream,
                 options = newParserOptions())
proc closeParser*(p: var TParser)

# implementation

proc OpenParser(p: var TParser, filename: string, 
                inputStream: PLLStream, options = newParserOptions()) = 
  OpenLexer(p.lex, filename, inputStream)
  p.options = options
  p.backtrack = @[]
  new(p.tok)

proc parMessage(p: TParser, msg: TMsgKind, arg = "") = 
  #assert false
  lexMessage(p.lex, msg, arg)

proc CloseParser(p: var TParser) = CloseLexer(p.lex)
proc saveContext(p: var TParser) = p.backtrack.add(p.tok)
proc closeContext(p: var TParser) = discard p.backtrack.pop()
proc backtrackContext(p: var TParser) = p.tok = p.backtrack.pop()

proc rawGetTok(p: var TParser) = 
  if p.tok.next != nil:
    p.tok = p.tok.next
  elif p.backtrack.len == 0: 
    p.tok.next = nil
    getTok(p.lex, p.tok[])
  else: 
    # We need the next token and must be able to backtrack. So we need to 
    # allocate a new token.
    var t: ref TToken
    new(t)
    getTok(p.lex, t[])
    p.tok.next = t
    p.tok = t

proc findMacro(p: TParser): int =
  for i in 0..high(p.options.macros):
    if p.tok.s == p.options.macros[i].name: return i
  return -1

proc rawEat(p: var TParser, xkind: TTokKind) = 
  if p.tok.xkind == xkind: rawGetTok(p)
  else: parMessage(p, errTokenExpected, TokKindToStr(xkind))

proc parseMacroArguments(p: var TParser): seq[seq[ref TToken]] = 
  result = @[]
  result.add(@[])
  var i: array[pxParLe..pxCurlyLe, int]
  var L = 0
  saveContext(p)
  while true:
    var kind = p.tok.xkind
    case kind
    of pxEof: rawEat(p, pxParRi)
    of pxParLe, pxBracketLe, pxCurlyLe: 
      inc(i[kind])
      result[L].add(p.tok)
    of pxParRi:
      # end of arguments?
      if i[pxParLe] == 0 and i[pxBracketLe] == 0 and i[pxCurlyLe] == 0: break
      if i[pxParLe] > 0: dec(i[pxParLe])
      result[L].add(p.tok)
    of pxBracketRi, pxCurlyRi:
      kind = pred(kind, 3)
      if i[kind] > 0: dec(i[kind])
      result[L].add(p.tok)
    of pxComma: 
      if i[pxParLe] == 0 and i[pxBracketLe] == 0 and i[pxCurlyLe] == 0:
        # next argument: comma is not part of the argument
        result.add(@[])
        inc(L)
      else: 
        # comma does not separate different arguments:
        result[L].add(p.tok)
    else:
      result[L].add(p.tok)
    rawGetTok(p)
  closeContext(p)

proc expandMacro(p: var TParser, m: TMacro) = 
  rawGetTok(p) # skip macro name
  var arguments: seq[seq[ref TToken]]
  if m.params > 0:
    rawEat(p, pxParLe)
    arguments = parseMacroArguments(p)
    if arguments.len != m.params: parMessage(p, errWrongNumberOfArguments)
    rawEat(p, pxParRi)
  # insert into the token list:
  if m.body.len > 0:
    var newList: ref TToken
    new(newList)
    var lastTok = newList
    for tok in items(m.body): 
      if tok.xkind == pxMacroParam: 
        for t in items(arguments[int(tok.iNumber)]):
          #echo "t: ", t^
          lastTok.next = t
          lastTok = t
      else:
        #echo "tok: ", tok^
        lastTok.next = tok
        lastTok = tok
    lastTok.next = p.tok
    p.tok = newList.next

proc getTok(p: var TParser) = 
  rawGetTok(p)
  if p.tok.xkind == pxSymbol:
    var idx = findMacro(p)
    if idx >= 0: 
      expandMacro(p, p.options.macros[idx])

proc parLineInfo(p: TParser): TLineInfo = 
  result = getLineInfo(p.lex)

proc skipComAux(p: var TParser, n: PNode) =
  if n != nil and n.kind != nkEmpty: 
    if pfSkipComments notin p.options.flags:
      if n.comment == nil: n.comment = p.tok.s
      else: add(n.comment, "\n" & p.tok.s)
  else: 
    parMessage(p, warnCommentXIgnored, p.tok.s)
  getTok(p)

proc skipCom(p: var TParser, n: PNode) = 
  while p.tok.xkind in {pxLineComment, pxStarComment}: skipcomAux(p, n)

proc skipStarCom(p: var TParser, n: PNode) = 
  while p.tok.xkind == pxStarComment: skipComAux(p, n)

proc getTok(p: var TParser, n: PNode) =
  getTok(p)
  skipCom(p, n)

proc ExpectIdent(p: TParser) = 
  if p.tok.xkind != pxSymbol: parMessage(p, errIdentifierExpected, $(p.tok[]))
  
proc Eat(p: var TParser, xkind: TTokKind, n: PNode) = 
  if p.tok.xkind == xkind: getTok(p, n)
  else: parMessage(p, errTokenExpected, TokKindToStr(xkind))
  
proc Eat(p: var TParser, xkind: TTokKind) = 
  if p.tok.xkind == xkind: getTok(p)
  else: parMessage(p, errTokenExpected, TokKindToStr(xkind))
  
proc Eat(p: var TParser, tok: string, n: PNode) = 
  if p.tok.s == tok: getTok(p, n)
  else: parMessage(p, errTokenExpected, tok)
  
proc Opt(p: var TParser, xkind: TTokKind, n: PNode) = 
  if p.tok.xkind == xkind: getTok(p, n)
  
proc addSon(father, a, b: PNode) = 
  addSon(father, a)
  addSon(father, b)

proc addSon(father, a, b, c: PNode) = 
  addSon(father, a)
  addSon(father, b)
  addSon(father, c)
  
proc newNodeP(kind: TNodeKind, p: TParser): PNode = 
  result = newNodeI(kind, getLineInfo(p.lex))

proc newIntNodeP(kind: TNodeKind, intVal: BiggestInt, p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.intVal = intVal

proc newFloatNodeP(kind: TNodeKind, floatVal: BiggestFloat, 
                   p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.floatVal = floatVal

proc newStrNodeP(kind: TNodeKind, strVal: string, p: TParser): PNode = 
  result = newNodeP(kind, p)
  result.strVal = strVal

proc newIdentNodeP(ident: PIdent, p: TParser): PNode = 
  result = newNodeP(nkIdent, p)
  result.ident = ident

proc newIdentNodeP(ident: string, p: TParser): PNode =
  result = newIdentNodeP(getIdent(ident), p)

proc mangleRules(s: string, p: TParser): string = 
  block mangle:
    for pattern, frmt in items(p.options.mangleRules):
      if s.match(pattern):
        result = s.replace(pattern, frmt)
        break mangle
    block prefixes:
      for prefix in items(p.options.prefixes): 
        if s.startsWith(prefix): 
          result = s.copy(prefix.len)
          break prefixes
      result = s
    block suffixes:
      for suffix in items(p.options.suffixes):
        if result.endsWith(suffix):
          setLen(result, result.len - suffix.len)
          break suffixes

proc mangleName(s: string, p: TParser): string = 
  if p.options.toMangle.hasKey(s): result = p.options.toMangle[s]
  else: result = mangleRules(s, p)

proc isPrivate(s: string, p: TParser): bool = 
  for pattern in items(p.options.privateRules): 
    if s.match(pattern): return true

proc mangledIdent(ident: string, p: TParser): PNode = 
  result = newNodeP(nkIdent, p)
  result.ident = getIdent(mangleName(ident, p))

proc newIdentPair(a, b: string, p: TParser): PNode = 
  result = newNodeP(nkExprColonExpr, p)
  addSon(result, newIdentNodeP(a, p))
  addSon(result, newIdentNodeP(b, p))

proc newIdentStrLitPair(a, b: string, p: TParser): PNode =
  result = newNodeP(nkExprColonExpr, p)
  addSon(result, newIdentNodeP(a, p))
  addSon(result, newStrNodeP(nkStrLit, b, p))

proc addImportToPragma(pragmas: PNode, ident: string, p: TParser) =
  addSon(pragmas, newIdentStrLitPair("importc", ident, p))
  if p.options.dynlibSym.len > 0:
    addSon(pragmas, newIdentPair("dynlib", p.options.dynlibSym, p))
  else:
    addSon(pragmas, newIdentStrLitPair("header", p.options.header, p))

proc exportSym(p: TParser, i: PNode, origName: string): PNode = 
  assert i.kind == nkIdent
  if p.scopeCounter == 0 and not isPrivate(origName, p):
    result = newNodeI(nkPostfix, i.info)
    addSon(result, newIdentNode(getIdent("*"), i.info), i)
  else:
    result = i

proc varIdent(ident: string, p: TParser): PNode = 
  result = exportSym(p, mangledIdent(ident, p), ident)
  if p.scopeCounter > 0: return
  if p.options.dynlibSym.len > 0 or p.options.header.len > 0: 
    var a = result
    result = newNodeP(nkPragmaExpr, p)
    var pragmas = newNodeP(nkPragma, p)
    addSon(result, a)
    addSon(result, pragmas)
    addImportToPragma(pragmas, ident, p)

proc fieldIdent(ident: string, p: TParser): PNode = 
  result = exportSym(p, mangledIdent(ident, p), ident)
  if p.scopeCounter > 0: return
  if p.options.header.len > 0: 
    var a = result
    result = newNodeP(nkPragmaExpr, p)
    var pragmas = newNodeP(nkPragma, p)
    addSon(result, a)
    addSon(result, pragmas)
    addSon(pragmas, newIdentStrLitPair("importc", ident, p))

proc DoImport(ident: string, pragmas: PNode, p: TParser) = 
  if p.options.dynlibSym.len > 0 or p.options.header.len > 0: 
    addImportToPragma(pragmas, ident, p)

proc newBinary(opr: string, a, b: PNode, p: TParser): PNode =
  result = newNodeP(nkInfix, p)
  addSon(result, newIdentNodeP(getIdent(opr), p))
  addSon(result, a)
  addSon(result, b)

proc skipIdent(p: var TParser): PNode = 
  expectIdent(p)
  result = mangledIdent(p.tok.s, p)
  getTok(p, result)

proc skipIdentExport(p: var TParser): PNode = 
  expectIdent(p)
  result = exportSym(p, mangledIdent(p.tok.s, p), p.tok.s)
  getTok(p, result)

proc skipTypeIdentExport(p: var TParser, prefix='T'): PNode = 
  expectIdent(p)
  var n = prefix & mangleName(p.tok.s, p)
  p.options.toMangle[p.tok.s] = n
  var i = newNodeP(nkIdent, p)
  i.ident = getIdent(n)
  result = exportSym(p, i, p.tok.s)
  getTok(p, result)

proc markTypeIdent(p: var TParser, typ: PNode) = 
  if pfTypePrefixes in p.options.flags:
    var prefix = ""
    if typ == nil or typ.kind == nkEmpty: 
      prefix = "T"
    else: 
      var t = typ
      while t != nil and t.kind in {nkVarTy, nkPtrTy, nkRefTy}: 
        prefix.add('P')
        t = t.sons[0]
      if prefix.len == 0: prefix.add('T')
    expectIdent(p)
    p.options.toMangle[p.tok.s] = prefix & mangleRules(p.tok.s, p)
  
# --------------- parser -----------------------------------------------------
# We use this parsing rule: If it looks like a declaration, it is one. This
# avoids to build a symbol table, which can't be done reliably anyway for our
# purposes.

proc expression(p: var TParser): PNode
proc constantExpression(p: var TParser): PNode
proc assignmentExpression(p: var TParser): PNode
proc compoundStatement(p: var TParser): PNode
proc statement(p: var TParser): PNode

proc declKeyword(s: string): bool = 
  # returns true if it is a keyword that introduces a declaration
  case s
  of  "extern", "static", "auto", "register", "const", "volatile", "restrict",
      "inline", "__inline", "__cdecl", "__stdcall", "__syscall", "__fastcall",
      "__safecall", "void", "struct", "union", "enum", "typedef",
      "short", "int", "long", "float", "double", "signed", "unsigned", "char": 
    result = true

proc stmtKeyword(s: string): bool =
  case s
  of  "if", "for", "while", "do", "switch", "break", "continue", "return",
      "goto":
    result = true

# ------------------- type desc -----------------------------------------------

proc isIntType(s: string): bool =
  case s
  of "short", "int", "long", "float", "double", "signed", "unsigned":
    result = true

proc skipConst(p: var TParser) = 
  while p.tok.xkind == pxSymbol and
      (p.tok.s == "const" or p.tok.s == "volatile" or p.tok.s == "restrict"): 
    getTok(p, nil)

proc typeAtom(p: var TParser): PNode = 
  skipConst(p)
  ExpectIdent(p)
  case p.tok.s
  of "void": 
    result = newNodeP(nkNilLit, p) # little hack
    getTok(p, nil)
  of "struct", "union", "enum": 
    getTok(p, nil)
    result = skipIdent(p)
  elif isIntType(p.tok.s):
    var x = "c" & p.tok.s
    getTok(p, nil)
    while p.tok.xkind == pxSymbol and 
        (isIntType(p.tok.s) or p.tok.s == "char"):
      add(x, p.tok.s)
      getTok(p, nil)
    result = mangledIdent(x, p)
  else: 
    result = mangledIdent(p.tok.s, p)
    getTok(p, result)
    
proc newPointerTy(p: TParser, typ: PNode): PNode =
  if pfRefs in p.options.flags: 
    result = newNodeP(nkRefTy, p)
  else:
    result = newNodeP(nkPtrTy, p)
  result.addSon(typ)
  
proc pointer(p: var TParser, a: PNode): PNode = 
  result = a
  var i = 0
  skipConst(p)
  while p.tok.xkind == pxStar:
    inc(i)
    getTok(p, result)
    skipConst(p)
    result = newPointerTy(p, result)
  if a.kind == nkIdent and a.ident.s == "char": 
    if i >= 2: 
      result = newIdentNodeP("cstringArray", p)
      for j in 1..i-2: result = newPointerTy(p, result)
    elif i == 1: result = newIdentNodeP("cstring", p)
  elif a.kind == nkNilLit and i > 0:
    result = newIdentNodeP("pointer", p)
    for j in 1..i-1: result = newPointerTy(p, result)

proc newProcPragmas(p: TParser): PNode =
  result = newNodeP(nkPragma, p)
  if pfCDecl in p.options.flags: 
    addSon(result, newIdentNodeP("cdecl", p))
  elif pfStdCall in p.options.flags:
    addSon(result, newIdentNodeP("stdcall", p))

proc addPragmas(father, pragmas: PNode) =
  if sonsLen(pragmas) > 0: addSon(father, pragmas)
  else: addSon(father, ast.emptyNode)

proc addReturnType(params, rettyp: PNode) =
  if rettyp == nil: addSon(params, ast.emptyNode)
  elif rettyp.kind != nkNilLit: addSon(params, rettyp)
  else: addson(params, ast.emptyNode)

proc parseFormalParams(p: var TParser, params, pragmas: PNode)

proc parseTypeSuffix(p: var TParser, typ: PNode): PNode = 
  result = typ
  while true:
    case p.tok.xkind 
    of pxBracketLe:
      getTok(p, result)
      skipConst(p) # POSIX contains: ``int [restrict]``
      if p.tok.xkind != pxBracketRi:
        var tmp = result
        var index = expression(p)
        # array type:
        result = newNodeP(nkBracketExpr, p)
        addSon(result, newIdentNodeP("array", p))
        var r = newNodeP(nkRange, p)
        addSon(r, newIntNodeP(nkIntLit, 0, p))
        addSon(r, newBinary("-", index, newIntNodeP(nkIntLit, 1, p), p))
        addSon(result, r)
        addSon(result, tmp)
      else:
        # pointer type:
        var tmp = result
        if pfRefs in p.options.flags: 
          result = newNodeP(nkRefTy, p)
        else:
          result = newNodeP(nkPtrTy, p)
        result.addSon(tmp)
      eat(p, pxBracketRi, result)
    of pxParLe:
      # function pointer:
      var procType = newNodeP(nkProcTy, p)
      var pragmas = newProcPragmas(p)
      var params = newNodeP(nkFormalParams, p)
      addReturnType(params, result)
      parseFormalParams(p, params, pragmas)
      addSon(procType, params)
      addPragmas(procType, pragmas)
      result = procType
    else: break

proc typeDesc(p: var TParser): PNode = 
  result = pointer(p, typeAtom(p))

proc parseField(p: var TParser, kind: TNodeKind): PNode =
  if p.tok.xkind == pxParLe: 
    getTok(p, nil)
    while p.tok.xkind == pxStar: getTok(p, nil)
    result = parseField(p, kind)
    eat(p, pxParRi, result)
  else: 
    expectIdent(p)
    if kind == nkRecList: result = fieldIdent(p.tok.s, p) 
    else: result = mangledIdent(p.tok.s, p)
    getTok(p, result)

proc takeOnlyFirstField(p: TParser, isUnion: bool): bool =
  # if we generate an interface to a header file, *all* fields can be 
  # generated:
  result = isUnion and p.options.header.len == 0
  
proc parseStructBody(p: var TParser, isUnion: bool,
                     kind: TNodeKind = nkRecList): PNode = 
  result = newNodeP(kind, p)
  eat(p, pxCurlyLe, result)
  while p.tok.xkind notin {pxEof, pxCurlyRi}:
    var baseTyp = typeAtom(p)
    while true:
      var def = newNodeP(nkIdentDefs, p)
      var t = pointer(p, baseTyp)
      var i = parseField(p, kind)
      t = parseTypeSuffix(p, t)
      addSon(def, i, t, ast.emptyNode)
      if not takeOnlyFirstField(p, isUnion) or sonsLen(result) < 1: 
        addSon(result, def)
      if p.tok.xkind != pxComma: break
      getTok(p, def)
    eat(p, pxSemicolon, lastSon(result))
  eat(p, pxCurlyRi, result)

proc structPragmas(p: TParser, name: PNode, origName: string): PNode = 
  assert name.kind == nkIdent
  result = newNodeP(nkPragmaExpr, p)
  addson(result, exportSym(p, name, origName))
  var pragmas = newNodep(nkPragma, p)
  addSon(pragmas, newIdentNodeP("pure", p), newIdentNodeP("final", p))
  if p.options.header.len > 0:
    addSon(pragmas, newIdentStrLitPair("importc", origName, p),
                    newIdentStrLitPair("header", p.options.header, p))
  addSon(result, pragmas)

proc enumPragmas(p: TParser, name: PNode): PNode =
  result = newNodeP(nkPragmaExpr, p)
  addson(result, name)
  var pragmas = newNodep(nkPragma, p)
  var e = newNodeP(nkExprColonExpr, p)
  # HACK: sizeof(cint) should be constructed as AST
  addSon(e, newIdentNodeP("size", p), newIdentNodeP("sizeof(cint)", p))
  addSon(pragmas, e)
  addSon(result, pragmas)

proc parseStruct(p: var TParser, isUnion: bool): PNode = 
  result = newNodeP(nkObjectTy, p)
  addSon(result, ast.emptyNode, ast.emptyNode) # no pragmas, no inheritance 
  if p.tok.xkind == pxCurlyLe:
    addSon(result, parseStructBody(p, isUnion))
  else: 
    addSon(result, newNodeP(nkRecList, p))

proc parseParam(p: var TParser, params: PNode) = 
  var typ = typeDesc(p)
  # support for ``(void)`` parameter list: 
  if typ.kind == nkNilLit and p.tok.xkind == pxParRi: return
  var name: PNode
  if p.tok.xkind == pxSymbol: 
    name = skipIdent(p)
  else:
    # generate a name for the formal parameter:
    var idx = sonsLen(params)+1
    name = newIdentNodeP("a" & $idx, p)
  typ = parseTypeSuffix(p, typ)
  var x = newNodeP(nkIdentDefs, p)
  addSon(x, name, typ)
  if p.tok.xkind == pxAsgn: 
    # we support default parameters for C++:
    getTok(p, x)
    addSon(x, assignmentExpression(p))
  else:
    addSon(x, ast.emptyNode)
  addSon(params, x)

proc parseFormalParams(p: var TParser, params, pragmas: PNode) = 
  eat(p, pxParLe, params)
  while p.tok.xkind notin {pxEof, pxParRi}:
    if p.tok.xkind == pxDotDotDot:  
      addSon(pragmas, newIdentNodeP("varargs", p))
      getTok(p, pragmas)
      break
    parseParam(p, params)
    if p.tok.xkind != pxComma: break
    getTok(p, params)
  eat(p, pxParRi, params)

proc parseCallConv(p: var TParser, pragmas: PNode) = 
  while p.tok.xkind == pxSymbol:
    case p.tok.s
    of "inline", "__inline": addSon(pragmas, newIdentNodeP("inline", p))
    of "__cdecl": addSon(pragmas, newIdentNodeP("cdecl", p))
    of "__stdcall": addSon(pragmas, newIdentNodeP("stdcall", p))
    of "__syscall": addSon(pragmas, newIdentNodeP("syscall", p))
    of "__fastcall": addSon(pragmas, newIdentNodeP("fastcall", p))
    of "__safecall": addSon(pragmas, newIdentNodeP("safecall", p))
    else: break
    getTok(p, nil)

proc parseFunctionPointerDecl(p: var TParser, rettyp: PNode): PNode = 
  var procType = newNodeP(nkProcTy, p)
  var pragmas = newProcPragmas(p)
  var params = newNodeP(nkFormalParams, p)
  eat(p, pxParLe, params)
  addReturnType(params, rettyp)
  parseCallConv(p, pragmas)
  if p.tok.xkind == pxStar: getTok(p, params)
  else: parMessage(p, errTokenExpected, "*")
  if p.inTypeDef > 0: markTypeIdent(p, nil)
  var name = skipIdentExport(p)
  eat(p, pxParRi, name)
  parseFormalParams(p, params, pragmas)
  addSon(procType, params)
  addPragmas(procType, pragmas)
  
  if p.inTypeDef == 0:
    result = newNodeP(nkVarSection, p)
    var def = newNodeP(nkIdentDefs, p)
    addSon(def, name, procType, ast.emptyNode)
    addSon(result, def)    
  else:
    result = newNodeP(nkTypeDef, p)
    addSon(result, name, ast.emptyNode, procType)
  assert result != nil
  
proc addTypeDef(section, name, t: PNode) = 
  var def = newNodeI(nkTypeDef, name.info)
  addSon(def, name, ast.emptyNode, t)
  addSon(section, def)
  
proc otherTypeDef(p: var TParser, section, typ: PNode) = 
  var name, t: PNode
  case p.tok.xkind
  of pxParLe: 
    # function pointer: typedef typ (*name)();
    var x = parseFunctionPointerDecl(p, typ)
    name = x[0]
    t = x[2]
  of pxStar:
    # typedef typ *b;
    t = pointer(p, typ)
    markTypeIdent(p, t)
    name = skipIdentExport(p)
  else: 
    # typedef typ name;
    t = typ
    markTypeIdent(p, t)
    name = skipIdentExport(p)
  t = parseTypeSuffix(p, t)
  addTypeDef(section, name, t)

proc parseTrailingDefinedTypes(p: var TParser, section, typ: PNode) = 
  while p.tok.xkind == pxComma:
    getTok(p, nil)
    var newTyp = pointer(p, typ)
    markTypeIdent(p, newTyp)
    var newName = skipIdentExport(p)
    newTyp = parseTypeSuffix(p, newTyp)
    addTypeDef(section, newName, newTyp)

proc enumFields(p: var TParser): PNode = 
  result = newNodeP(nkEnumTy, p)
  addSon(result, ast.emptyNode) # enum does not inherit from anything
  while true:
    var e = skipIdent(p)
    if p.tok.xkind == pxAsgn: 
      getTok(p, e)
      var c = constantExpression(p)
      var a = e
      e = newNodeP(nkEnumFieldDef, p)
      addSon(e, a, c)
      skipCom(p, e)
    
    addSon(result, e)
    if p.tok.xkind != pxComma: break
    getTok(p, e)
    # allow trailing comma:
    if p.tok.xkind == pxCurlyRi: break

proc parseTypedefStruct(p: var TParser, result: PNode, isUnion: bool) = 
  getTok(p, result)
  if p.tok.xkind == pxCurlyLe:
    var t = parseStruct(p, isUnion)
    var origName = p.tok.s
    markTypeIdent(p, nil)
    var name = skipIdent(p)
    addTypeDef(result, structPragmas(p, name, origName), t)
    parseTrailingDefinedTypes(p, result, name)
  elif p.tok.xkind == pxSymbol: 
    # name to be defined or type "struct a", we don't know yet:
    markTypeIdent(p, nil)
    var origName = p.tok.s
    var nameOrType = skipIdent(p)
    case p.tok.xkind 
    of pxCurlyLe:
      var t = parseStruct(p, isUnion)
      if p.tok.xkind == pxSymbol: 
        # typedef struct tagABC {} abc, *pabc;
        # --> abc is a better type name than tagABC!
        markTypeIdent(p, nil)
        var origName = p.tok.s
        var name = skipIdent(p)
        addTypeDef(result, structPragmas(p, name, origName), t)
        parseTrailingDefinedTypes(p, result, name)
      else:
        addTypeDef(result, structPragmas(p, nameOrType, origName), t)
    of pxSymbol: 
      # typedef struct a a?
      if mangleName(p.tok.s, p) == nameOrType.ident.s:
        # ignore the declaration:
        getTok(p, nil)
      else:
        # typedef struct a b; or typedef struct a b[45];
        otherTypeDef(p, result, nameOrType)
    else: 
      otherTypeDef(p, result, nameOrType)
  else:
    expectIdent(p)

proc parseTypedefEnum(p: var TParser, result: PNode) = 
  getTok(p, result)
  if p.tok.xkind == pxCurlyLe:
    getTok(p, result)
    var t = enumFields(p)
    eat(p, pxCurlyRi, t)
    var origName = p.tok.s
    markTypeIdent(p, nil)
    var name = skipIdent(p)
    addTypeDef(result, enumPragmas(p, exportSym(p, name, origName)), t)
    parseTrailingDefinedTypes(p, result, name)
  elif p.tok.xkind == pxSymbol: 
    # name to be defined or type "enum a", we don't know yet:
    markTypeIdent(p, nil)
    var origName = p.tok.s
    var nameOrType = skipIdent(p)
    case p.tok.xkind 
    of pxCurlyLe:
      getTok(p, result)
      var t = enumFields(p)
      eat(p, pxCurlyRi, t)
      if p.tok.xkind == pxSymbol: 
        # typedef enum tagABC {} abc, *pabc;
        # --> abc is a better type name than tagABC!
        markTypeIdent(p, nil)
        var origName = p.tok.s
        var name = skipIdent(p)
        addTypeDef(result, enumPragmas(p, exportSym(p, name, origName)), t)
        parseTrailingDefinedTypes(p, result, name)
      else:
        addTypeDef(result, 
                   enumPragmas(p, exportSym(p, nameOrType, origName)), t)
    of pxSymbol: 
      # typedef enum a a?
      if mangleName(p.tok.s, p) == nameOrType.ident.s:
        # ignore the declaration:
        getTok(p, nil)
      else:
        # typedef enum a b; or typedef enum a b[45];
        otherTypeDef(p, result, nameOrType)
    else: 
      otherTypeDef(p, result, nameOrType)
  else:
    expectIdent(p)

proc parseTypeDef(p: var TParser): PNode =  
  result = newNodeP(nkTypeSection, p)
  while p.tok.xkind == pxSymbol and p.tok.s == "typedef":
    getTok(p, result)
    inc(p.inTypeDef)
    expectIdent(p)
    case p.tok.s
    of "struct": parseTypedefStruct(p, result, isUnion=false)
    of "union": parseTypedefStruct(p, result, isUnion=true)
    of "enum": parseTypedefEnum(p, result)
    else: 
      var t = typeAtom(p)
      otherTypeDef(p, result, t)
    eat(p, pxSemicolon)
    dec(p.inTypeDef)
    
proc skipDeclarationSpecifiers(p: var TParser) =
  while p.tok.xkind == pxSymbol:
    case p.tok.s
    of "extern", "static", "auto", "register", "const", "volatile": 
      getTok(p, nil)
    else: break

proc parseInitializer(p: var TParser): PNode = 
  if p.tok.xkind == pxCurlyLe: 
    result = newNodeP(nkBracket, p)
    getTok(p, result)
    while p.tok.xkind notin {pxEof, pxCurlyRi}: 
      addSon(result, parseInitializer(p))
      opt(p, pxComma, nil)
    eat(p, pxCurlyRi, result)
  else:
    result = assignmentExpression(p)

proc addInitializer(p: var TParser, def: PNode) = 
  if p.tok.xkind == pxAsgn:
    getTok(p, def)
    addSon(def, parseInitializer(p))
  else:
    addSon(def, ast.emptyNode)  

proc parseVarDecl(p: var TParser, baseTyp, typ: PNode, 
                  origName: string): PNode =  
  result = newNodeP(nkVarSection, p)
  var def = newNodeP(nkIdentDefs, p)
  addSon(def, varIdent(origName, p))
  addSon(def, parseTypeSuffix(p, typ))
  addInitializer(p, def)
  addSon(result, def)
    
  while p.tok.xkind == pxComma: 
    getTok(p, def)
    var t = pointer(p, baseTyp)
    expectIdent(p)
    def = newNodeP(nkIdentDefs, p)
    addSon(def, varIdent(p.tok.s, p))
    getTok(p, def)
    addSon(def, parseTypeSuffix(p, t))
    addInitializer(p, def)
    addSon(result, def)
  eat(p, pxSemicolon)

when false:
  proc declaration(p: var TParser, father: PNode) =
    # general syntax to parse is::
    #
    #   baseType ::= typeIdent | ((struct|union|enum) ident ("{" body "}" )?
    #                                                      | "{" body "}")
    #   declIdent ::= "(" "*" ident ")" formalParams ("=" exprNoComma)?
    #               | ident ((formalParams ("{" statements "}")?)|"=" 
    #                        exprNoComma|(typeSuffix("=" exprNoComma)? ))?
    #   declaration ::= baseType (pointers)? declIdent ("," declIdent)*
    var pragmas = newNodeP(nkPragma, p)
    
    skipDeclarationSpecifiers(p)
    parseCallConv(p, pragmas)
    skipDeclarationSpecifiers(p)
    expectIdent(p)  

proc declaration(p: var TParser): PNode = 
  result = newNodeP(nkProcDef, p)
  var pragmas = newNodeP(nkPragma, p)
  
  skipDeclarationSpecifiers(p)
  parseCallConv(p, pragmas)
  skipDeclarationSpecifiers(p)
  expectIdent(p)
  var baseTyp = typeAtom(p)
  var rettyp = pointer(p, baseTyp)
  skipDeclarationSpecifiers(p)
  parseCallConv(p, pragmas)
  skipDeclarationSpecifiers(p)
  
  if p.tok.xkind == pxParLe: 
    # Function pointer declaration: This is of course only a heuristic, but the
    # best we can do here.
    result = parseFunctionPointerDecl(p, rettyp)
    eat(p, pxSemicolon)
    return
  ExpectIdent(p)
  var origName = p.tok.s
  getTok(p) # skip identifier
  case p.tok.xkind 
  of pxParLe: 
    # really a function!
    var name = mangledIdent(origName, p)
    var params = newNodeP(nkFormalParams, p)
    addReturnType(params, rettyp)
    parseFormalParams(p, params, pragmas)
    
    if pfCDecl in p.options.flags:
      addSon(pragmas, newIdentNodeP("cdecl", p))
    elif pfStdcall in p.options.flags:
      addSon(pragmas, newIdentNodeP("stdcall", p))
    addSon(result, exportSym(p, name, origName), ast.emptyNode) # no generics
    addSon(result, params, pragmas)
    case p.tok.xkind 
    of pxSemicolon: 
      getTok(p)
      addSon(result, ast.emptyNode) # nobody
      if p.scopeCounter == 0: DoImport(origName, pragmas, p)
    of pxCurlyLe:
      addSon(result, compoundStatement(p))
    else:
      parMessage(p, errTokenExpected, ";")
    if sonsLen(result.sons[pragmasPos]) == 0: 
      result.sons[pragmasPos] = ast.emptyNode
  of pxAsgn, pxSemicolon, pxComma:
    result = parseVarDecl(p, baseTyp, rettyp, origName)
  else:
    parMessage(p, errTokenExpected, ";")
  assert result != nil

proc createConst(name, typ, val: PNode, p: TParser): PNode =
  result = newNodeP(nkConstDef, p)
  addSon(result, name, typ, val)

proc enumSpecifier(p: var TParser): PNode =  
  saveContext(p)
  getTok(p, nil) # skip "enum"
  case p.tok.xkind
  of pxCurlyLe: 
    closeContext(p)
    # make a const section out of it:
    result = newNodeP(nkConstSection, p)
    getTok(p, result)
    var i = 0
    while true:
      var name = skipIdentExport(p)
      var val: PNode
      if p.tok.xkind == pxAsgn: 
        getTok(p, name)
        val = constantExpression(p)
        if val.kind == nkIntLit: i = int(val.intVal)+1
        else: parMessage(p, errXExpected, "int literal")
      else:
        val = newIntNodeP(nkIntLit, i, p)
        inc(i)
      var c = createConst(name, ast.emptyNode, val, p)
      addSon(result, c)
      if p.tok.xkind != pxComma: break
      getTok(p, c)
      # allow trailing comma:
      if p.tok.xkind == pxCurlyRi: break
    eat(p, pxCurlyRi, result)
    eat(p, pxSemicolon)
  of pxSymbol: 
    var origName = p.tok.s
    markTypeIdent(p, nil)
    result = skipIdent(p)
    case p.tok.xkind 
    of pxCurlyLe: 
      closeContext(p)
      var name = result
      # create a type section containing the enum
      result = newNodeP(nkTypeSection, p)
      var t = newNodeP(nkTypeDef, p)
      getTok(p, t)
      var e = enumFields(p)
      addSon(t, exportSym(p, name, origName), ast.emptyNode, e)
      addSon(result, t)
      eat(p, pxCurlyRi, result)
      eat(p, pxSemicolon)
    of pxSemicolon:
      # just ignore ``enum X;`` for now.
      closeContext(p)
      getTok(p, nil)
    else: 
      backtrackContext(p)
      result = declaration(p)
  else:
    closeContext(p)
    parMessage(p, errTokenExpected, "{")
    result = ast.emptyNode
    
# Expressions

proc setBaseFlags(n: PNode, base: TNumericalBase) = 
  case base
  of base10: nil
  of base2: incl(n.flags, nfBase2)
  of base8: incl(n.flags, nfBase8)
  of base16: incl(n.flags, nfBase16)

proc unaryExpression(p: var TParser): PNode

proc isDefinitelyAType(p: var TParser): bool = 
  var starFound = false
  var words = 0
  while true:
    case p.tok.xkind 
    of pxSymbol:
      if declKeyword(p.tok.s): return true
      elif starFound: return false
      else: inc(words)
    of pxStar:
      starFound = true
    of pxParRi: return words == 0 or words > 1 or starFound
    else: return false
    getTok(p, nil)

proc castExpression(p: var TParser): PNode = 
  if p.tok.xkind == pxParLe: 
    saveContext(p)
    result = newNodeP(nkCast, p)
    getTok(p, result)
    var t = isDefinitelyAType(p)
    backtrackContext(p)
    if t:
      eat(p, pxParLe, result)
      var a = typeDesc(p)
      eat(p, pxParRi, result)
      addSon(result, a)
      addSon(result, castExpression(p))
    else: 
      # else it is just an expression in ():
      result = newNodeP(nkPar, p)
      eat(p, pxParLe, result)
      addSon(result, expression(p))
      if p.tok.xkind != pxParRi:  
        # ugh, it is a cast, even though it does not look like one:
        result.kind = nkCast
        addSon(result, castExpression(p))
      eat(p, pxParRi, result)
      #result = unaryExpression(p)
  else:
    result = unaryExpression(p)
  
proc primaryExpression(p: var TParser): PNode = 
  case p.tok.xkind
  of pxSymbol: 
    if p.tok.s == "NULL": 
      result = newNodeP(nkNilLit, p)
    else: 
      result = mangledIdent(p.tok.s, p)
    getTok(p, result)
  of pxIntLit: 
    result = newIntNodeP(nkIntLit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p, result)
  of pxInt64Lit: 
    result = newIntNodeP(nkInt64Lit, p.tok.iNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p, result)
  of pxFloatLit: 
    result = newFloatNodeP(nkFloatLit, p.tok.fNumber, p)
    setBaseFlags(result, p.tok.base)
    getTok(p, result)
  of pxStrLit: 
    # Ansi C allows implicit string literal concatenations:
    result = newStrNodeP(nkStrLit, p.tok.s, p)
    getTok(p, result)
    while p.tok.xkind == pxStrLit:
      add(result.strVal, p.tok.s)
      getTok(p, result)
  of pxCharLit:
    result = newIntNodeP(nkCharLit, ord(p.tok.s[0]), p)
    getTok(p, result)
  of pxParLe:
    result = castExpression(p)
  else:
    result = ast.emptyNode

proc multiplicativeExpression(p: var TParser): PNode = 
  result = castExpression(p)
  while true:
    case p.tok.xkind
    of pxStar:
      var a = result
      result = newNodeP(nkInfix, p)
      addSon(result, newIdentNodeP("*", p), a)
      getTok(p, result)
      var b = castExpression(p)
      addSon(result, b)
    of pxSlash:
      var a = result
      result = newNodeP(nkInfix, p)
      addSon(result, newIdentNodeP("div", p), a)
      getTok(p, result)
      var b = castExpression(p)
      addSon(result, b)
    of pxMod:
      var a = result
      result = newNodeP(nkInfix, p)
      addSon(result, newIdentNodeP("mod", p), a)
      getTok(p, result)
      var b = castExpression(p)
      addSon(result, b)
    else: break 

proc additiveExpression(p: var TParser): PNode = 
  result = multiplicativeExpression(p)
  while true:
    case p.tok.xkind
    of pxPlus:
      var a = result
      result = newNodeP(nkInfix, p)
      addSon(result, newIdentNodeP("+", p), a)
      getTok(p, result)
      var b = multiplicativeExpression(p)
      addSon(result, b)
    of pxMinus:
      var a = result
      result = newNodeP(nkInfix, p)
      addSon(result, newIdentNodeP("-", p), a)
      getTok(p, result)
      var b = multiplicativeExpression(p)
      addSon(result, b)
    else: break 
  
proc incdec(p: var TParser, opr: string): PNode = 
  result = newNodeP(nkCall, p)
  addSon(result, newIdentNodeP(opr, p))
  gettok(p, result)
  addSon(result, unaryExpression(p))

proc unaryOp(p: var TParser, kind: TNodeKind): PNode = 
  result = newNodeP(kind, p)
  getTok(p, result)
  addSon(result, castExpression(p))

proc prefixCall(p: var TParser, opr: string): PNode = 
  result = newNodeP(nkPrefix, p)
  addSon(result, newIdentNodeP(opr, p))
  gettok(p, result)
  addSon(result, castExpression(p))

proc postfixExpression(p: var TParser): PNode = 
  result = primaryExpression(p)
  while true:
    case p.tok.xkind
    of pxBracketLe:
      var a = result
      result = newNodeP(nkBracketExpr, p)
      addSon(result, a)
      getTok(p, result)
      var b = expression(p)
      addSon(result, b)
      eat(p, pxBracketRi, result)
    of pxParLe:
      var a = result
      result = newNodeP(nkCall, p)
      addSon(result, a)
      getTok(p, result)
      if p.tok.xkind != pxParRi:
        a = assignmentExpression(p)
        addSon(result, a)
        while p.tok.xkind == pxComma:
          getTok(p, a)
          a = assignmentExpression(p)
          addSon(result, a)
      eat(p, pxParRi, result)
    of pxDot, pxArrow:
      var a = result
      result = newNodeP(nkDotExpr, p)
      addSon(result, a)
      getTok(p, result)
      addSon(result, skipIdent(p))
    of pxPlusPlus:
      var a = result
      result = newNodeP(nkCall, p)
      addSon(result, newIdentNodeP("inc", p))
      gettok(p, result)
      addSon(result, a)
    of pxMinusMinus:
      var a = result
      result = newNodeP(nkCall, p)
      addSon(result, newIdentNodeP("dec", p))
      gettok(p, result)
      addSon(result, a)
    else: break

proc unaryExpression(p: var TParser): PNode =
  case p.tok.xkind
  of pxPlusPlus: result = incdec(p, "inc")
  of pxMinusMinus: result = incdec(p, "dec")
  of pxAmp: result = unaryOp(p, nkAddr)
  of pxStar: result = unaryOp(p, nkBracketExpr)
  of pxPlus: result = prefixCall(p, "+")
  of pxMinus: result = prefixCall(p, "-")
  of pxTilde: result = prefixCall(p, "not")
  of pxNot: result = prefixCall(p, "not")
  of pxSymbol:
    if p.tok.s == "sizeof": 
      result = newNodeP(nkCall, p)
      addSon(result, newIdentNodeP("sizeof", p))
      getTok(p, result)
      if p.tok.xkind == pxParLe: 
        getTok(p, result)
        addson(result, typeDesc(p))
        eat(p, pxParRi, result)
      else:
        addSon(result, unaryExpression(p))
    else:
      result = postfixExpression(p)
  else: result = postfixExpression(p)

proc expression(p: var TParser): PNode = 
  # we cannot support C's ``,`` operator
  result = assignmentExpression(p)
  if p.tok.xkind == pxComma:
    getTok(p, result)
    parMessage(p, errOperatorExpected, ",")
    
proc conditionalExpression(p: var TParser): PNode

proc constantExpression(p: var TParser): PNode = 
  result = conditionalExpression(p)

proc lvalue(p: var TParser): PNode = 
  result = unaryExpression(p)

proc asgnExpr(p: var TParser, opr: string, a: PNode): PNode = 
  closeContext(p)
  getTok(p, a)
  var b = assignmentExpression(p)
  result = newNodeP(nkAsgn, p)
  addSon(result, a, newBinary(opr, copyTree(a), b, p))
  
proc incdec(p: var TParser, opr: string, a: PNode): PNode =
  closeContext(p)
  getTok(p, a)
  var b = assignmentExpression(p)
  result = newNodeP(nkCall, p)
  addSon(result, newIdentNodeP(getIdent(opr), p), a, b)
  
proc assignmentExpression(p: var TParser): PNode = 
  saveContext(p)
  var a = lvalue(p)
  case p.tok.xkind 
  of pxAsgn:
    closeContext(p)
    getTok(p, a)
    var b = assignmentExpression(p)
    result = newNodeP(nkAsgn, p)
    addSon(result, a, b)
  of pxPlusAsgn: result = incDec(p, "inc", a)    
  of pxMinusAsgn: result = incDec(p, "dec", a)
  of pxStarAsgn: result = asgnExpr(p, "*", a)
  of pxSlashAsgn: result = asgnExpr(p, "/", a)
  of pxModAsgn: result = asgnExpr(p, "mod", a)
  of pxShlAsgn: result = asgnExpr(p, "shl", a)
  of pxShrAsgn: result = asgnExpr(p, "shr", a)
  of pxAmpAsgn: result = asgnExpr(p, "and", a)
  of pxHatAsgn: result = asgnExpr(p, "xor", a)
  of pxBarAsgn: result = asgnExpr(p, "or", a)
  else:
    backtrackContext(p)
    result = conditionalExpression(p)
  
proc shiftExpression(p: var TParser): PNode = 
  result = additiveExpression(p)
  while p.tok.xkind in {pxShl, pxShr}:
    var op = if p.tok.xkind == pxShl: "shl" else: "shr"
    getTok(p, result)
    var a = result 
    var b = additiveExpression(p)
    result = newBinary(op, a, b, p)

proc relationalExpression(p: var TParser): PNode = 
  result = shiftExpression(p)
  # Nimrod uses ``<`` and ``<=``, etc. too:
  while p.tok.xkind in {pxLt, pxLe, pxGt, pxGe}:
    var op = TokKindToStr(p.tok.xkind)
    getTok(p, result)
    var a = result 
    var b = shiftExpression(p)
    result = newBinary(op, a, b, p)

proc equalityExpression(p: var TParser): PNode =
  result = relationalExpression(p)
  # Nimrod uses ``==`` and ``!=`` too:
  while p.tok.xkind in {pxEquals, pxNeq}:
    var op = TokKindToStr(p.tok.xkind)
    getTok(p, result)
    var a = result 
    var b = relationalExpression(p)
    result = newBinary(op, a, b, p)

proc andExpression(p: var TParser): PNode =
  result = equalityExpression(p)
  while p.tok.xkind == pxAmp:
    getTok(p, result)
    var a = result 
    var b = equalityExpression(p)
    result = newBinary("and", a, b, p)

proc exclusiveOrExpression(p: var TParser): PNode = 
  result = andExpression(p)
  while p.tok.xkind == pxHat:
    getTok(p, result)
    var a = result 
    var b = andExpression(p)
    result = newBinary("^", a, b, p)

proc inclusiveOrExpression(p: var TParser): PNode = 
  result = exclusiveOrExpression(p)
  while p.tok.xkind == pxBar:
    getTok(p, result)
    var a = result 
    var b = exclusiveOrExpression(p)
    result = newBinary("or", a, b, p)
  
proc logicalAndExpression(p: var TParser): PNode = 
  result = inclusiveOrExpression(p)
  while p.tok.xkind == pxAmpAmp:
    getTok(p, result)
    var a = result
    var b = inclusiveOrExpression(p)
    result = newBinary("and", a, b, p)

proc logicalOrExpression(p: var TParser): PNode = 
  result = logicalAndExpression(p)
  while p.tok.xkind == pxBarBar:
    getTok(p, result)
    var a = result
    var b = logicalAndExpression(p)
    result = newBinary("or", a, b, p)
  
proc conditionalExpression(p: var TParser): PNode =  
  result = logicalOrExpression(p)
  if p.tok.xkind == pxConditional: 
    getTok(p, result) # skip '?'
    var a = result
    var b = expression(p)
    eat(p, pxColon, b)
    var c = conditionalExpression(p)
    result = newNodeP(nkIfExpr, p)
    var branch = newNodeP(nkElifExpr, p)
    addSon(branch, a, b)
    addSon(result, branch)
    branch = newNodeP(nkElseExpr, p)
    addSon(branch, c)
    addSon(result, branch)
    
# Statements

proc buildStmtList(a: PNode): PNode = 
  if a.kind == nkStmtList: result = a
  else:
    result = newNodeI(nkStmtList, a.info)
    addSon(result, a)

proc nestedStatement(p: var TParser): PNode =
  # careful: We need to translate:
  # if (x) if (y) stmt;
  # into:
  # if x:
  #   if x:
  #     stmt
  # 
  # Nimrod requires complex statements to be nested in whitespace!
  const
    complexStmt = {nkProcDef, nkMethodDef, nkConverterDef, nkMacroDef,
      nkTemplateDef, nkIteratorDef, nkMacroStmt, nkIfStmt,
      nkWhenStmt, nkForStmt, nkWhileStmt, nkCaseStmt, nkVarSection, 
      nkConstSection, nkTypeSection, nkTryStmt, nkBlockStmt, nkStmtList,
      nkCommentStmt, nkStmtListExpr, nkBlockExpr, nkStmtListType, nkBlockType}
  result = statement(p)
  if result.kind in complexStmt:
    result = buildStmtList(result)

proc expressionStatement(p: var TParser): PNode = 
  # do not skip the comment after a semicolon to make a new nkCommentStmt
  if p.tok.xkind == pxSemicolon: 
    getTok(p)
    result = ast.emptyNode
  else:
    result = expression(p)
    if p.tok.xkind == pxSemicolon: getTok(p)
    else: parMessage(p, errTokenExpected, ";")
  assert result != nil

proc parseIf(p: var TParser): PNode = 
  # we parse additional "else if"s too here for better Nimrod code
  result = newNodeP(nkIfStmt, p)
  while true: 
    getTok(p) # skip ``if``
    var branch = newNodeP(nkElifBranch, p)
    skipCom(p, branch)
    eat(p, pxParLe, branch)
    addSon(branch, expression(p))
    eat(p, pxParRi, branch)
    addSon(branch, nestedStatement(p))
    addSon(result, branch)
    if p.tok.s == "else": 
      getTok(p, result)
      if p.tok.s != "if": 
        # ordinary else part:
        branch = newNodeP(nkElse, p)
        addSon(branch, nestedStatement(p))
        addSon(result, branch)
        break 
    else: 
      break 
  
proc parseWhile(p: var TParser): PNode = 
  result = newNodeP(nkWhileStmt, p)
  getTok(p, result)
  eat(p, pxParLe, result)
  addSon(result, expression(p))
  eat(p, pxParRi, result)
  addSon(result, nestedStatement(p))

proc parseDoWhile(p: var TParser): PNode =  
  # we only support ``do stmt while (0)`` as an idiom for 
  # ``block: stmt``
  result = newNodeP(nkBlockStmt, p)
  getTok(p, result) # skip "do"
  addSon(result, ast.emptyNode, nestedStatement(p))
  eat(p, "while", result)
  eat(p, pxParLe, result)
  if p.tok.xkind == pxIntLit and p.tok.iNumber == 0: getTok(p, result)
  else: parMessage(p, errTokenExpected, "0")
  eat(p, pxParRi, result)
  if p.tok.xkind == pxSemicolon: getTok(p)

proc declarationOrStatement(p: var TParser): PNode = 
  if p.tok.xkind != pxSymbol:
    result = expressionStatement(p)
  elif declKeyword(p.tok.s): 
    result = declaration(p)
  else:
    # ordinary identifier:
    saveContext(p)
    getTok(p) # skip identifier to look ahead
    case p.tok.xkind 
    of pxSymbol, pxStar: 
      # we parse 
      # a b
      # a * b
      # always as declarations! This is of course not correct, but good
      # enough for most real world C code out there.
      backtrackContext(p)
      result = declaration(p)
    of pxColon: 
      # it is only a label:
      closeContext(p)
      getTok(p)
      result = statement(p)
    else: 
      backtrackContext(p)
      result = expressionStatement(p)
  assert result != nil

proc parseTuple(p: var TParser, isUnion: bool): PNode = 
  result = parseStructBody(p, isUnion, nkTupleTy)

proc parseTrailingDefinedIdents(p: var TParser, result, baseTyp: PNode) =
  var varSection = newNodeP(nkVarSection, p)
  while p.tok.xkind notin {pxEof, pxSemicolon}:
    var t = pointer(p, baseTyp)
    expectIdent(p)
    var def = newNodeP(nkIdentDefs, p)
    addSon(def, varIdent(p.tok.s, p))
    getTok(p, def)
    addSon(def, parseTypeSuffix(p, t))
    addInitializer(p, def)
    addSon(varSection, def)
    if p.tok.xkind != pxComma: break
    getTok(p, def)
  eat(p, pxSemicolon)
  if sonsLen(varSection) > 0:
    addSon(result, varSection)

proc parseStandaloneStruct(p: var TParser, isUnion: bool): PNode = 
  result = newNodeP(nkStmtList, p)
  saveContext(p)
  getTok(p, result) # skip "struct" or "union"
  var origName = ""
  if p.tok.xkind == pxSymbol: 
    markTypeIdent(p, nil)
    origName = p.tok.s
    getTok(p, result)
  if p.tok.xkind in {pxCurlyLe, pxSemiColon}:
    if origName.len > 0: 
      var name = mangledIdent(origName, p)
      var t = parseStruct(p, isUnion)
      var typeSection = newNodeP(nkTypeSection, p)
      addTypeDef(typeSection, structPragmas(p, name, origName), t)
      addSon(result, typeSection)
      parseTrailingDefinedIdents(p, result, name)
    else:
      var t = parseTuple(p, isUnion)
      parseTrailingDefinedIdents(p, result, t)
  else:
    backtrackContext(p)
    result = declaration(p)

proc parseFor(p: var TParser, result: PNode) = 
  # 'for' '(' expression_statement expression_statement expression? ')'
  #   statement
  getTok(p, result)
  eat(p, pxParLe, result)
  var initStmt = declarationOrStatement(p)
  if initStmt.kind != nkEmpty:
    addSon(result, initStmt)
  var w = newNodeP(nkWhileStmt, p)
  var condition = expressionStatement(p)
  if condition.kind == nkEmpty: condition = newIdentNodeP("true", p)
  addSon(w, condition)
  var step = if p.tok.xkind != pxParRi: expression(p) else: ast.emptyNode
  eat(p, pxParRi, step)
  var loopBody = nestedStatement(p)
  if step.kind != nkEmpty:
    loopBody = buildStmtList(loopBody)
    addSon(loopBody, step)
  addSon(w, loopBody)
  addSon(result, w)
  
proc switchStatement(p: var TParser): PNode = 
  result = newNodeP(nkStmtList, p)
  while true:
    if p.tok.xkind in {pxEof, pxCurlyRi}: break
    case p.tok.s 
    of "break":
      getTok(p, result)
      eat(p, pxSemicolon, result)
      break
    of "return", "continue", "goto": 
      addSon(result, statement(p))
      break
    of "case", "default":
      break
    else: nil
    addSon(result, statement(p))
  if sonsLen(result) == 0:
    # translate empty statement list to Nimrod's ``nil`` statement
    result = newNodeP(nkNilLit, p)

proc rangeExpression(p: var TParser): PNode =
  # We support GCC's extension: ``case expr...expr:`` 
  result = constantExpression(p)
  if p.tok.xkind == pxDotDotDot:
    getTok(p, result)
    var a = result
    var b = constantExpression(p)
    result = newNodeP(nkRange, p)
    addSon(result, a)
    addSon(result, b)

proc parseSwitch(p: var TParser): PNode = 
  # We cannot support Duff's device or C's crazy switch syntax. We just support
  # sane usages of switch. ;-)
  result = newNodeP(nkCaseStmt, p)
  getTok(p, result)
  eat(p, pxParLe, result)
  addSon(result, expression(p))
  eat(p, pxParRi, result)
  eat(p, pxCurlyLe, result)
  var b: PNode
  while (p.tok.xkind != pxCurlyRi) and (p.tok.xkind != pxEof): 
    case p.tok.s 
    of "default": 
      b = newNodeP(nkElse, p)
      getTok(p, b)
      eat(p, pxColon, b)
    of "case": 
      b = newNodeP(nkOfBranch, p)
      while p.tok.xkind == pxSymbol and p.tok.s == "case":
        getTok(p, b)
        addSon(b, rangeExpression(p))
        eat(p, pxColon, b)
    else:
      parMessage(p, errXExpected, "case")
    addSon(b, switchStatement(p))
    addSon(result, b)
    if b.kind == nkElse: break 
  eat(p, pxCurlyRi)

proc addStmt(sl, a: PNode) = 
  # merge type sections is possible:
  if a.kind != nkTypeSection or sonsLen(sl) == 0 or
      lastSon(sl).kind != nkTypeSection:
    addSon(sl, a)
  else:
    var ts = lastSon(sl)
    for i in 0..sonsLen(a)-1: addSon(ts, a.sons[i])

proc embedStmts(sl, a: PNode) = 
  if a.kind != nkStmtList:
    addStmt(sl, a)
  else:
    for i in 0..sonsLen(a)-1: 
      if a[i].kind != nkEmpty: addStmt(sl, a[i])

proc compoundStatement(p: var TParser): PNode = 
  result = newNodeP(nkStmtList, p)
  eat(p, pxCurlyLe)
  inc(p.scopeCounter)
  while p.tok.xkind notin {pxEof, pxCurlyRi}: 
    var a = statement(p)
    if a.kind == nkEmpty: break
    embedStmts(result, a)
  if sonsLen(result) == 0:
    # translate ``{}`` to Nimrod's ``nil`` statement
    result = newNodeP(nkNilLit, p)
  dec(p.scopeCounter)
  eat(p, pxCurlyRi)

include cpp

proc statement(p: var TParser): PNode = 
  case p.tok.xkind 
  of pxSymbol: 
    case p.tok.s
    of "if": result = parseIf(p)
    of "switch": result = parseSwitch(p)
    of "while": result = parseWhile(p)
    of "do": result = parseDoWhile(p)
    of "for": 
      result = newNodeP(nkStmtList, p)
      parseFor(p, result)
    of "goto": 
      # we cannot support "goto"; in hand-written C, "goto" is most often used
      # to break a block, so we convert it to a break statement with label.
      result = newNodeP(nkBreakStmt, p)
      getTok(p)
      addSon(result, skipIdent(p))
      eat(p, pxSemicolon)
    of "continue":
      result = newNodeP(nkContinueStmt, p)
      getTok(p)
      eat(p, pxSemicolon)
      addSon(result, ast.emptyNode)
    of "break":
      result = newNodeP(nkBreakStmt, p)
      getTok(p)
      eat(p, pxSemicolon)
      addSon(result, ast.emptyNode)
    of "return":
      result = newNodeP(nkReturnStmt, p)
      getTok(p)
      # special case for ``return (expr)`` because I hate the redundant
      # parenthesis ;-)
      if p.tok.xkind == pxParLe:
        getTok(p, result)
        addSon(result, expression(p))
        eat(p, pxParRi, result)
      elif p.tok.xkind != pxSemicolon:
        addSon(result, expression(p))
      else:
        addSon(result, ast.emptyNode)
      eat(p, pxSemicolon)
    of "enum": result = enumSpecifier(p)
    of "typedef": result = parseTypeDef(p)
    of "struct": result = parseStandaloneStruct(p, isUnion=false)
    of "union": result = parseStandaloneStruct(p, isUnion=true)    
    else: result = declarationOrStatement(p)
  of pxCurlyLe:
    result = compoundStatement(p)
  of pxDirective, pxDirectiveParLe:
    result = parseDir(p)
  of pxLineComment, pxStarComment: 
    result = newNodeP(nkCommentStmt, p)
    skipCom(p, result)
  of pxSemicolon:
    # empty statement:
    getTok(p)
    if p.tok.xkind in {pxLineComment, pxStarComment}:
      result = newNodeP(nkCommentStmt, p)
      skipCom(p, result)
    else:
      result = newNodeP(nkNilLit, p)
  else:
    result = expressionStatement(p)
  assert result != nil

proc parseUnit(p: var TParser): PNode = 
  result = newNodeP(nkStmtList, p)
  getTok(p) # read first token
  while p.tok.xkind != pxEof: 
    var s = statement(p)
    if s.kind != nkEmpty: embedStmts(result, s)

