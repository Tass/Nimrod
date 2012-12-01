#
#
#           The Nimrod Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module implements the symbol importing mechanism.

import 
  intsets, strutils, os, ast, astalgo, msgs, options, idents, rodread, lookups,
  semdata, passes, renderer

proc evalImport*(c: PContext, n: PNode): PNode
proc evalFrom*(c: PContext, n: PNode): PNode

proc getModuleName*(n: PNode): string =
  # This returns a short relative module name without the nim extension
  # e.g. like "system", "importer" or "somepath/module"
  # The proc won't perform any checks that the path is actually valid
  case n.kind
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    result = UnixToNativePath(n.strVal)
  of nkIdent:
    result = n.ident.s
  of nkSym:
    result = n.sym.name.s
  else:
    # hacky way to implement 'x / y /../ z':
    result = renderTree(n, {renderNoComments}).replace(" ")
    #localError(n.info, errGenerated,
    #  "invalide module name: '$1'" % renderTree(n))
    #result = ""

proc checkModuleName*(n: PNode): string =
  # This returns the full canonical path for a given module import
  var modulename = n.getModuleName
  result = findModule(modulename)
  if result.len == 0:
    LocalError(n.info, errCannotOpenFile, modulename)

proc rawImportSymbol(c: PContext, s: PSym) =
  # This does not handle stubs, because otherwise loading on demand would be
  # pointless in practice. So importing stubs is fine here!
  # check if we have already a symbol of the same name:
  var check = StrTableGet(c.tab.stack[importTablePos], s.name)
  if check != nil and check.id != s.id:
    if s.kind notin OverloadableSyms:
      # s and check need to be qualified:
      Incl(c.AmbiguousSymbols, s.id)
      Incl(c.AmbiguousSymbols, check.id)
  # thanks to 'export' feature, it could be we import the same symbol from
  # multiple sources, so we need to call 'StrTableAdd' here:
  StrTableAdd(c.tab.stack[importTablePos], s)
  if s.kind == skType:
    var etyp = s.typ
    if etyp.kind in {tyBool, tyEnum} and sfPure notin s.flags:
      for j in countup(0, sonsLen(etyp.n) - 1):
        var e = etyp.n.sons[j].sym
        if e.Kind != skEnumField: 
          InternalError(s.info, "rawImportSymbol") 
          # BUGFIX: because of aliases for enums the symbol may already
          # have been put into the symbol table
          # BUGFIX: but only iff they are the same symbols!
        var it: TIdentIter 
        check = InitIdentIter(it, c.tab.stack[importTablePos], e.name)
        while check != nil:
          if check.id == e.id:
            e = nil
            break
          check = NextIdentIter(it, c.tab.stack[importTablePos])
        if e != nil:
          rawImportSymbol(c, e)
  else:
    # rodgen assures that converters and patterns are no stubs
    if s.kind == skConverter: addConverter(c, s)
    if hasPattern(s): addPattern(c, s)

proc importSymbol(c: PContext, n: PNode, fromMod: PSym) = 
  let ident = lookups.considerAcc(n)
  let s = StrTableGet(fromMod.tab, ident)
  if s == nil:
    LocalError(n.info, errUndeclaredIdentifier, ident.s)
  else:
    if s.kind == skStub: loadStub(s)
    if s.Kind notin ExportableSymKinds:
      InternalError(n.info, "importSymbol: 2")
    # for an enumeration we have to add all identifiers
    case s.Kind
    of skProc, skMethod, skIterator, skMacro, skTemplate, skConverter:
      # for a overloadable syms add all overloaded routines
      var it: TIdentIter
      var e = InitIdentIter(it, fromMod.tab, s.name)
      while e != nil:
        if e.name.id != s.Name.id: InternalError(n.info, "importSymbol: 3")
        rawImportSymbol(c, e)
        e = NextIdentIter(it, fromMod.tab)
    else: rawImportSymbol(c, s)

proc importAllSymbolsExcept(c: PContext, fromMod: PSym, exceptSet: TIntSet) =
  var i: TTabIter
  var s = InitTabIter(i, fromMod.tab)
  while s != nil:
    if s.kind != skModule:
      if s.kind != skEnumField:
        if s.Kind notin ExportableSymKinds:
          InternalError(s.info, "importAllSymbols: " & $s.kind)
        if exceptSet.empty or s.name.id notin exceptSet:
          rawImportSymbol(c, s)
    s = NextIter(i, fromMod.tab)

proc importAllSymbols*(c: PContext, fromMod: PSym) =
  var exceptSet: TIntSet
  importAllSymbolsExcept(c, fromMod, exceptSet)

proc importForwarded(c: PContext, n: PNode, exceptSet: TIntSet) =
  if n.isNil: return
  case n.kind
  of nkExportStmt:
    for a in n:
      assert a.kind == nkSym
      let s = a.sym
      if s.kind == skModule:
        importAllSymbolsExcept(c, s, exceptSet)
      elif exceptSet.empty or s.name.id notin exceptSet:
        rawImportSymbol(c, s)
  of nkExportExceptStmt:
    localError(n.info, errGenerated, "'export except' not implemented")
  else:
    for i in 0 ..safeLen(n)-1:
      importForwarded(c, n.sons[i], exceptSet)

proc evalImport(c: PContext, n: PNode): PNode = 
  result = n
  var emptySet: TIntSet
  for i in countup(0, sonsLen(n) - 1): 
    var f = checkModuleName(n.sons[i])
    if f.len > 0:
      var m = gImportModule(f)
      if sfDeprecated in m.flags: 
        Message(n.sons[i].info, warnDeprecated, m.name.s) 
      # ``addDecl`` needs to be done before ``importAllSymbols``!
      addDecl(c, m)             # add symbol to symbol table of module
      importAllSymbolsExcept(c, m, emptySet)
      importForwarded(c, m.ast, emptySet)

proc evalFrom(c: PContext, n: PNode): PNode = 
  result = n
  checkMinSonsLen(n, 2)
  var f = checkModuleName(n.sons[0])
  if f.len > 0:
    var m = gImportModule(f)
    n.sons[0] = newSymNode(m)
    addDecl(c, m)               # add symbol to symbol table of module
    for i in countup(1, sonsLen(n) - 1): importSymbol(c, n.sons[i], m)

proc evalImportExcept*(c: PContext, n: PNode): PNode = 
  result = n
  checkMinSonsLen(n, 2)
  var f = checkModuleName(n.sons[0])
  if f.len > 0:
    var m = gImportModule(f)
    n.sons[0] = newSymNode(m)
    addDecl(c, m)               # add symbol to symbol table of module
    var exceptSet = initIntSet()
    for i in countup(1, sonsLen(n) - 1): 
      let ident = lookups.considerAcc(n.sons[i])
      exceptSet.incl(ident.id)
    importAllSymbolsExcept(c, m, exceptSet)
    importForwarded(c, m.ast, exceptSet)
