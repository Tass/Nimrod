#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## this module does the semantic checking of statements
#  included from sem.nim

proc semCommand(c: PContext, n: PNode): PNode =
  result = semExprNoType(c, n)

proc semWhen(c: PContext, n: PNode, semCheck = true): PNode =
  # If semCheck is set to false, ``when`` will return the verbatim AST of
  # the correct branch. Otherwise the AST will be passed through semStmt.
  result = nil
  
  template setResult(e: expr) =
    if semCheck: result = semStmt(c, e) # do not open a new scope!
    else: result = e

  for i in countup(0, sonsLen(n) - 1): 
    var it = n.sons[i]
    case it.kind
    of nkElifBranch, nkElifExpr: 
      checkSonsLen(it, 2)
      var e = semConstExpr(c, it.sons[0])
      if e.kind != nkIntLit: InternalError(n.info, "semWhen")
      elif e.intVal != 0 and result == nil:
        setResult(it.sons[1]) 
    of nkElse, nkElseExpr:
      checkSonsLen(it, 1)
      if result == nil: 
        setResult(it.sons[0])
    else: illFormedAst(n)
  if result == nil: 
    result = newNodeI(nkNilLit, n.info) 
  # The ``when`` statement implements the mechanism for platform dependent
  # code. Thus we try to ensure here consistent ID allocation after the
  # ``when`` statement.
  IDsynchronizationPoint(200)

proc semIf(c: PContext, n: PNode): PNode = 
  result = n
  for i in countup(0, sonsLen(n) - 1): 
    var it = n.sons[i]
    case it.kind
    of nkElifBranch: 
      checkSonsLen(it, 2)
      it.sons[0] = forceBool(c, semExprWithType(c, it.sons[0]))
      openScope(c.tab)
      it.sons[1] = semStmt(c, it.sons[1])
      closeScope(c.tab)
    of nkElse: 
      if sonsLen(it) == 1: it.sons[0] = semStmtScope(c, it.sons[0])
      else: illFormedAst(it)
    else: illFormedAst(n)
  
proc semDiscard(c: PContext, n: PNode): PNode = 
  result = n
  checkSonsLen(n, 1)
  if n.sons[0].kind != nkEmpty:
    n.sons[0] = semExprWithType(c, n.sons[0])
    if isEmptyType(n.sons[0].typ): localError(n.info, errInvalidDiscard)
  
proc semBreakOrContinue(c: PContext, n: PNode): PNode =
  result = n
  checkSonsLen(n, 1)
  if n.sons[0].kind != nkEmpty: 
    var s: PSym
    case n.sons[0].kind
    of nkIdent: s = lookUp(c, n.sons[0])
    of nkSym: s = n.sons[0].sym
    else: illFormedAst(n)
    if s.kind == skLabel and s.owner.id == c.p.owner.id: 
      var x = newSymNode(s)
      x.info = n.info
      incl(s.flags, sfUsed)
      n.sons[0] = x
      suggestSym(x, s)
    else:
      localError(n.info, errInvalidControlFlowX, s.name.s)
  elif (c.p.nestedLoopCounter <= 0) and (c.p.nestedBlockCounter <= 0): 
    localError(n.info, errInvalidControlFlowX, 
               renderTree(n, {renderNoComments}))
  
proc semBlock(c: PContext, n: PNode): PNode = 
  result = n
  Inc(c.p.nestedBlockCounter)
  checkSonsLen(n, 2)
  openScope(c.tab)            # BUGFIX: label is in the scope of block!
  if n.sons[0].kind != nkEmpty:
    var labl = newSymG(skLabel, n.sons[0], c)
    if sfGenSym notin labl.flags:
      addDecl(c, labl)
      n.sons[0] = newSymNode(labl, n.sons[0].info)
    suggestSym(n.sons[0], labl)
  n.sons[1] = semStmt(c, n.sons[1])
  closeScope(c.tab)
  Dec(c.p.nestedBlockCounter)

proc semAsm(con: PContext, n: PNode): PNode = 
  checkSonsLen(n, 2)
  var marker = pragmaAsm(con, n.sons[0])
  if marker == '\0': marker = '`' # default marker
  result = semAsmOrEmit(con, n, marker)
  
proc semWhile(c: PContext, n: PNode): PNode = 
  result = n
  checkSonsLen(n, 2)
  openScope(c.tab)
  n.sons[0] = forceBool(c, semExprWithType(c, n.sons[0]))
  inc(c.p.nestedLoopCounter)
  n.sons[1] = semStmt(c, n.sons[1])
  dec(c.p.nestedLoopCounter)
  closeScope(c.tab)

proc toCover(t: PType): biggestInt = 
  var t2 = skipTypes(t, abstractVarRange)
  if t2.kind == tyEnum and enumHasHoles(t2): 
    result = sonsLen(t2.n)
  else:
    result = lengthOrd(skipTypes(t, abstractVar))

proc semCase(c: PContext, n: PNode): PNode = 
  # check selector:
  result = n
  checkMinSonsLen(n, 2)
  openScope(c.tab)
  n.sons[0] = semExprWithType(c, n.sons[0])
  var chckCovered = false
  var covered: biggestint = 0
  case skipTypes(n.sons[0].Typ, abstractVarRange).Kind
  of tyInt..tyInt64, tyChar, tyEnum: 
    chckCovered = true
  of tyFloat..tyFloat128, tyString, tyError: 
    nil
  else: 
    LocalError(n.info, errSelectorMustBeOfCertainTypes)
    return
  for i in countup(1, sonsLen(n) - 1): 
    var x = n.sons[i]
    case x.kind
    of nkOfBranch: 
      checkMinSonsLen(x, 2)
      semCaseBranch(c, n, x, i, covered)
      var length = sonsLen(x)
      x.sons[length - 1] = semStmtScope(c, x.sons[length - 1])
    of nkElifBranch: 
      chckCovered = false
      checkSonsLen(x, 2)
      x.sons[0] = forceBool(c, semExprWithType(c, x.sons[0]))
      x.sons[1] = semStmtScope(c, x.sons[1])
    of nkElse: 
      chckCovered = false
      checkSonsLen(x, 1)
      x.sons[0] = semStmtScope(c, x.sons[0])
    else: illFormedAst(x)
  if chckCovered and (covered != toCover(n.sons[0].typ)): 
    localError(n.info, errNotAllCasesCovered)
  closeScope(c.tab)
  
proc fitRemoveHiddenConv(c: PContext, typ: Ptype, n: PNode): PNode = 
  result = fitNode(c, typ, n)
  if result.kind in {nkHiddenStdConv, nkHiddenSubConv}: 
    changeType(result.sons[1], typ)
    result = result.sons[1]
  elif not sameType(result.typ, typ): 
    changeType(result, typ)

proc findShadowedVar(c: PContext, v: PSym): PSym =
  for i in countdown(c.tab.tos - 2, ModuleTablePos+1):
    let shadowed = StrTableGet(c.tab.stack[i], v.name)
    if shadowed != nil and shadowed.kind in skLocalVars:
      return shadowed

proc identWithin(n: PNode, s: PIdent): bool =
  for i in 0 .. n.safeLen-1:
    if identWithin(n.sons[i], s): return true
  result = n.kind == nkSym and n.sym.name.id == s.id

proc semIdentDef(c: PContext, n: PNode, kind: TSymKind): PSym =
  if isTopLevel(c): 
    result = semIdentWithPragma(c, kind, n, {sfExported})
    incl(result.flags, sfGlobal)
  else:
    result = semIdentWithPragma(c, kind, n, {})
  suggestSym(n, result)

proc semVarOrLet(c: PContext, n: PNode, symkind: TSymKind): PNode = 
  var b: PNode
  result = copyNode(n)
  for i in countup(0, sonsLen(n)-1): 
    var a = n.sons[i]
    if gCmd == cmdIdeTools: suggestStmt(c, a)
    if a.kind == nkCommentStmt: continue 
    if a.kind notin {nkIdentDefs, nkVarTuple, nkConstDef}: IllFormedAst(a)
    checkMinSonsLen(a, 3)
    var length = sonsLen(a)
    var typ: PType
    if a.sons[length-2].kind != nkEmpty:
      typ = semTypeNode(c, a.sons[length-2], nil)
    else: 
      typ = nil
    var def: PNode
    if a.sons[length-1].kind != nkEmpty: 
      def = semExprWithType(c, a.sons[length-1])
      # BUGFIX: ``fitNode`` is needed here!
      # check type compability between def.typ and typ:
      if typ != nil: def = fitNode(c, typ, def)
      else: typ = skipIntLit(def.typ)
    else:
      def = ast.emptyNode
      if symkind == skLet: LocalError(a.info, errLetNeedsInit)
      
    # this can only happen for errornous var statements:
    if typ == nil: continue
    if not typeAllowed(typ, symkind): 
      LocalError(a.info, errXisNoType, typeToString(typ))
    var tup = skipTypes(typ, {tyGenericInst})
    if a.kind == nkVarTuple: 
      if tup.kind != tyTuple: 
        localError(a.info, errXExpected, "tuple")
      elif length-2 != sonsLen(tup): 
        localError(a.info, errWrongNumberOfVariables)
      else:
        b = newNodeI(nkVarTuple, a.info)
        newSons(b, length)
        b.sons[length-2] = a.sons[length-2] # keep type desc for doc generator
        b.sons[length-1] = def
        addSon(result, b)
    elif tup.kind == tyTuple and def.kind == nkPar and 
        a.kind == nkIdentDefs and a.len > 3:
      Message(a.info, warnEachIdentIsTuple)
    for j in countup(0, length-3):
      var v = semIdentDef(c, a.sons[j], symkind)
      if sfGenSym notin v.flags: addInterfaceDecl(c, v)
      when oKeepVariableNames:
        if c.InUnrolledContext > 0: v.flags.incl(sfShadowed)
        else:
          let shadowed = findShadowedVar(c, v)
          if shadowed != nil:
            shadowed.flags.incl(sfShadowed)
            # a shadowed variable is an error unless it appears on the right
            # side of the '=':
            if warnShadowIdent in gNotes and not identWithin(def, v.name):
              Message(a.info, warnShadowIdent, v.name.s)
      if def != nil and def.kind != nkEmpty:
        # this is only needed for the evaluation pass:
        v.ast = def
        if sfThread in v.flags: LocalError(def.info, errThreadvarCannotInit)
      if a.kind != nkVarTuple:
        v.typ = typ
        b = newNodeI(nkIdentDefs, a.info)
        if importantComments():
          # keep documentation information:
          b.comment = a.comment
        addSon(b, newSymNode(v))
        addSon(b, a.sons[length-2])      # keep type desc for doc generator
        addSon(b, copyTree(def))
        addSon(result, b)
      else: 
        v.typ = tup.sons[j]
        b.sons[j] = newSymNode(v)
    
proc semConst(c: PContext, n: PNode): PNode = 
  result = copyNode(n)
  for i in countup(0, sonsLen(n) - 1): 
    var a = n.sons[i]
    if gCmd == cmdIdeTools: suggestStmt(c, a)
    if a.kind == nkCommentStmt: continue 
    if (a.kind != nkConstDef): IllFormedAst(a)
    checkSonsLen(a, 3)
    var v = semIdentDef(c, a.sons[0], skConst)
    var typ: PType = nil
    if a.sons[1].kind != nkEmpty: typ = semTypeNode(c, a.sons[1], nil)

    var def = semConstExpr(c, a.sons[2])
    if def == nil:
      LocalError(a.sons[2].info, errConstExprExpected)
      continue
    # check type compatibility between def.typ and typ:
    if typ != nil:
      def = fitRemoveHiddenConv(c, typ, def)
    else:
      typ = def.typ
    if typ == nil: continue
    if not typeAllowed(typ, skConst):
      LocalError(a.info, errXisNoType, typeToString(typ))
      continue
    v.typ = typ
    v.ast = def               # no need to copy
    if sfGenSym notin v.flags: addInterfaceDecl(c, v)
    var b = newNodeI(nkConstDef, a.info)
    if importantComments(): b.comment = a.comment
    addSon(b, newSymNode(v))
    addSon(b, ast.emptyNode)            # no type description
    addSon(b, copyTree(def))
    addSon(result, b)

type
  TFieldInstCtx = object  # either 'tup[i]' or 'field' is valid
    tupleType: PType      # if != nil we're traversing a tuple
    tupleIndex: int
    field: PSym
    replaceByFieldName: bool

proc instFieldLoopBody(c: TFieldInstCtx, n: PNode, forLoop: PNode): PNode =
  case n.kind
  of nkEmpty..pred(nkIdent), succ(nkIdent)..nkNilLit: result = n
  of nkIdent:
    result = n
    var L = sonsLen(forLoop)
    if c.replaceByFieldName:
      if n.ident.id == forLoop[0].ident.id:
        let fieldName = if c.tupleType.isNil: c.field.name.s
                        elif c.tupleType.n.isNil: "Field" & $c.tupleIndex
                        else: c.tupleType.n.sons[c.tupleIndex].sym.name.s
        result = newStrNode(nkStrLit, fieldName)
        return
    # other fields:
    for i in ord(c.replaceByFieldName)..L-3:
      if n.ident.id == forLoop[i].ident.id:
        var call = forLoop.sons[L-2]
        var tupl = call.sons[i+1-ord(c.replaceByFieldName)]
        if c.field.isNil:
          result = newNodeI(nkBracketExpr, n.info)
          result.add(tupl)
          result.add(newIntNode(nkIntLit, c.tupleIndex))
        else:
          result = newNodeI(nkDotExpr, n.info)
          result.add(tupl)
          result.add(newSymNode(c.field, n.info))
        break
  else:
    if n.kind == nkContinueStmt:
      localError(n.info, errGenerated,
                 "'continue' not supported in a 'fields' loop")
    result = copyNode(n)
    newSons(result, sonsLen(n))
    for i in countup(0, sonsLen(n)-1):
      result.sons[i] = instFieldLoopBody(c, n.sons[i], forLoop)

type
  TFieldsCtx = object
    c: PContext
    m: TMagic

proc semForObjectFields(c: TFieldsCtx, typ, forLoop, father: PNode) =
  case typ.kind
  of nkSym:
    var fc: TFieldInstCtx  # either 'tup[i]' or 'field' is valid
    fc.field = typ.sym
    fc.replaceByFieldName = c.m == mFieldPairs
    openScope(c.c.tab)
    inc c.c.InUnrolledContext
    let body = instFieldLoopBody(fc, lastSon(forLoop), forLoop)
    father.add(SemStmt(c.c, body))
    dec c.c.InUnrolledContext
    closeScope(c.c.tab)
  of nkNilLit: nil
  of nkRecCase:
    let L = forLoop.len
    let call = forLoop.sons[L-2]
    if call.len > 2:
      LocalError(forLoop.info, errGenerated, 
                 "parallel 'fields' iterator does not work for 'case' objects")
      return
    # iterate over the selector:
    semForObjectFields(c, typ[0], forLoop, father)
    # we need to generate a case statement:
    var caseStmt = newNodeI(nkCaseStmt, forLoop.info)
    # generate selector:
    var access = newNodeI(nkDotExpr, forLoop.info, 2)
    access.sons[0] = call.sons[1]
    access.sons[1] = newSymNode(typ.sons[0].sym, forLoop.info)
    caseStmt.add(semExprWithType(c.c, access))
    # copy the branches over, but replace the fields with the for loop body:
    for i in 1 .. <typ.len:
      var branch = copyTree(typ[i])
      let L = branch.len
      branch.sons[L-1] = newNodeI(nkStmtList, forLoop.info)
      semForObjectFields(c, typ[i].lastSon, forLoop, branch[L-1])
      caseStmt.add(branch)
    father.add(caseStmt)
  of nkRecList:
    for t in items(typ): semForObjectFields(c, t, forLoop, father)
  else:
    illFormedAst(typ)

proc semForFields(c: PContext, n: PNode, m: TMagic): PNode =
  # so that 'break' etc. work as expected, we produce
  # a 'while true: stmt; break' loop ...
  result = newNodeI(nkWhileStmt, n.info, 2)
  var trueSymbol = StrTableGet(magicsys.systemModule.Tab, getIdent"true")
  if trueSymbol == nil: 
    LocalError(n.info, errSystemNeeds, "true")
    trueSymbol = newSym(skUnknown, getIdent"true", getCurrOwner(), n.info)
    trueSymbol.typ = getSysType(tyBool)

  result.sons[0] = newSymNode(trueSymbol, n.info)
  var stmts = newNodeI(nkStmtList, n.info)
  result.sons[1] = stmts
  
  var length = sonsLen(n)
  var call = n.sons[length-2]
  if length-2 != sonsLen(call)-1 + ord(m==mFieldPairs):
    LocalError(n.info, errWrongNumberOfVariables)
    return result
  
  var tupleTypeA = skipTypes(call.sons[1].typ, abstractVar)
  if tupleTypeA.kind notin {tyTuple, tyObject}:
    localError(n.info, errGenerated, "no object or tuple type")
    return result
  for i in 1..call.len-1:
    var tupleTypeB = skipTypes(call.sons[i].typ, abstractVar)
    if not SameType(tupleTypeA, tupleTypeB):
      typeMismatch(call.sons[i], tupleTypeA, tupleTypeB)
  
  Inc(c.p.nestedLoopCounter)
  if tupleTypeA.kind == tyTuple:
    var loopBody = n.sons[length-1]
    for i in 0..sonsLen(tupleTypeA)-1:
      openScope(c.tab)
      var fc: TFieldInstCtx
      fc.tupleType = tupleTypeA
      fc.tupleIndex = i
      fc.replaceByFieldName = m == mFieldPairs
      var body = instFieldLoopBody(fc, loopBody, n)
      inc c.InUnrolledContext
      stmts.add(SemStmt(c, body))
      dec c.InUnrolledContext
      closeScope(c.tab)
  else:
    var fc: TFieldsCtx
    fc.m = m
    fc.c = c
    semForObjectFields(fc, tupleTypeA.n, n, stmts)
  Dec(c.p.nestedLoopCounter)
  var b = newNodeI(nkBreakStmt, n.info)
  b.add(ast.emptyNode)
  stmts.add(b)

proc addForVarDecl(c: PContext, v: PSym) =
  if warnShadowIdent in gNotes:
    let shadowed = findShadowedVar(c, v)
    if shadowed != nil:
      # XXX should we do this here?
      #shadowed.flags.incl(sfShadowed)
      Message(v.info, warnShadowIdent, v.name.s)
  addDecl(c, v)

proc semForVars(c: PContext, n: PNode): PNode =
  result = n
  var length = sonsLen(n)
  var iter = skipTypes(n.sons[length-2].typ, {tyGenericInst})
  # length == 3 means that there is one for loop variable
  # and thus no tuple unpacking:
  if iter.kind != tyTuple or length == 3: 
    if length == 3:
      var v = newSymG(skForVar, n.sons[0], c)
      if getCurrOwner().kind == skModule: incl(v.flags, sfGlobal)
      # BUGFIX: don't use `iter` here as that would strip away
      # the ``tyGenericInst``! See ``tests/compile/tgeneric.nim``
      # for an example:
      v.typ = n.sons[length-2].typ
      n.sons[0] = newSymNode(v)
      if sfGenSym notin v.flags: addForVarDecl(c, v)
    else:
      LocalError(n.info, errWrongNumberOfVariables)
  elif length-2 != sonsLen(iter):
    LocalError(n.info, errWrongNumberOfVariables)
  else:
    for i in countup(0, length - 3): 
      var v = newSymG(skForVar, n.sons[i], c)
      if getCurrOwner().kind == skModule: incl(v.flags, sfGlobal)
      v.typ = iter.sons[i]
      n.sons[i] = newSymNode(v)
      if sfGenSym notin v.flags: addForVarDecl(c, v)
  Inc(c.p.nestedLoopCounter)
  n.sons[length-1] = SemStmt(c, n.sons[length-1])
  Dec(c.p.nestedLoopCounter)

proc newDeref(n: PNode): PNode {.inline.} =  
  result = newNodeIT(nkHiddenDeref, n.info, n.typ.sons[0])
  addSon(result, n)

proc implicitIterator(c: PContext, it: string, arg: PNode): PNode =
  result = newNodeI(nkCall, arg.info)
  result.add(newIdentNode(it.getIdent, arg.info))
  if arg.typ != nil and arg.typ.kind == tyVar: 
    result.add newDeref(arg)
  else:
    result.add arg
  result = semExprNoDeref(c, result, {efWantIterator})

proc semFor(c: PContext, n: PNode): PNode = 
  result = n
  checkMinSonsLen(n, 3)
  var length = sonsLen(n)
  openScope(c.tab)
  n.sons[length-2] = semExprNoDeref(c, n.sons[length-2], {efWantIterator})
  var call = n.sons[length-2]
  if call.kind in nkCallKinds and call.sons[0].typ.callConv == ccClosure:
    # first class iterator:
    result = semForVars(c, n)
  elif call.kind notin nkCallKinds or call.sons[0].kind != nkSym or
      call.sons[0].sym.kind != skIterator: 
    if length == 3:
      n.sons[length-2] = implicitIterator(c, "items", n.sons[length-2])
    elif length == 4:
      n.sons[length-2] = implicitIterator(c, "pairs", n.sons[length-2])
    else:
      LocalError(n.sons[length-2].info, errIteratorExpected)
    result = semForVars(c, n)
  elif call.sons[0].sym.magic != mNone:
    if call.sons[0].sym.magic == mOmpParFor:
      result = semForVars(c, n)
      result.kind = nkParForStmt
    else:
      result = semForFields(c, n, call.sons[0].sym.magic)
  else:
    result = semForVars(c, n)
  closeScope(c.tab)

proc semRaise(c: PContext, n: PNode): PNode = 
  result = n
  checkSonsLen(n, 1)
  if n.sons[0].kind != nkEmpty: 
    n.sons[0] = semExprWithType(c, n.sons[0])
    var typ = n.sons[0].typ
    if typ.kind != tyRef or typ.sons[0].kind != tyObject: 
      localError(n.info, errExprCannotBeRaised)

proc semTry(c: PContext, n: PNode): PNode = 
  result = n
  inc c.p.inTryStmt
  checkMinSonsLen(n, 2)
  n.sons[0] = semStmtScope(c, n.sons[0])
  var check = initIntSet()
  for i in countup(1, sonsLen(n) - 1): 
    var a = n.sons[i]
    checkMinSonsLen(a, 1)
    var length = sonsLen(a)
    if a.kind == nkExceptBranch:
      if length == 2 and a.sons[0].kind == nkBracket:
        a.sons[0..0] = a.sons[0].sons
        length = a.sonsLen

      for j in countup(0, length - 2):
        var typ = semTypeNode(c, a.sons[j], nil)
        if typ.kind == tyRef: typ = typ.sons[0]
        if typ.kind != tyObject:
          LocalError(a.sons[j].info, errExprCannotBeRaised)
        a.sons[j] = newNodeI(nkType, a.sons[j].info)
        a.sons[j].typ = typ
        if ContainsOrIncl(check, typ.id):
          localError(a.sons[j].info, errExceptionAlreadyHandled)
    elif a.kind != nkFinally: 
      illFormedAst(n) 
    # last child of an nkExcept/nkFinally branch is a statement:
    a.sons[length - 1] = semStmtScope(c, a.sons[length - 1])
  dec c.p.inTryStmt

proc addGenericParamListToScope(c: PContext, n: PNode) =
  if n.kind != nkGenericParams: illFormedAst(n)
  for i in countup(0, sonsLen(n)-1):
    var a = n.sons[i]
    if a.kind == nkSym: addDecl(c, a.sym)
    else: illFormedAst(a)

proc typeSectionLeftSidePass(c: PContext, n: PNode) = 
  # process the symbols on the left side for the whole type section, before
  # we even look at the type definitions on the right
  for i in countup(0, sonsLen(n) - 1): 
    var a = n.sons[i]
    if gCmd == cmdIdeTools: suggestStmt(c, a)
    if a.kind == nkCommentStmt: continue 
    if a.kind != nkTypeDef: IllFormedAst(a)
    checkSonsLen(a, 3)
    var s = semIdentDef(c, a.sons[0], skType)
    s.typ = newTypeS(tyForward, c)
    s.typ.sym = s             # process pragmas:
    if a.sons[0].kind == nkPragmaExpr:
      pragma(c, s, a.sons[0].sons[1], typePragmas)
    # add it here, so that recursive types are possible:
    if sfGenSym notin s.flags: addInterfaceDecl(c, s)
    a.sons[0] = newSymNode(s)

proc typeSectionRightSidePass(c: PContext, n: PNode) =
  for i in countup(0, sonsLen(n) - 1): 
    var a = n.sons[i]
    if a.kind == nkCommentStmt: continue 
    if (a.kind != nkTypeDef): IllFormedAst(a)
    checkSonsLen(a, 3)
    if (a.sons[0].kind != nkSym): IllFormedAst(a)
    var s = a.sons[0].sym
    if s.magic == mNone and a.sons[2].kind == nkEmpty: 
      LocalError(a.info, errImplOfXexpected, s.name.s)
    if s.magic != mNone: processMagicType(c, s)
    if a.sons[1].kind != nkEmpty: 
      # We have a generic type declaration here. In generic types,
      # symbol lookup needs to be done here.
      openScope(c.tab)
      pushOwner(s)
      if s.magic == mNone: s.typ.kind = tyGenericBody
      if s.typ.containerID != 0: 
        InternalError(a.info, "semTypeSection: containerID")
      s.typ.containerID = s.typ.id
      # XXX for generic type aliases this is not correct! We need the
      # underlying Id really: 
      #
      # type
      #   TGObj[T] = object
      #   TAlias[T] = TGObj[T]
      # 
      a.sons[1] = semGenericParamList(c, a.sons[1], s.typ)
      s.typ.size = -1 # could not be computed properly
      # we fill it out later. For magic generics like 'seq', it won't be filled
      # so we use tyEmpty instead of nil to not crash for strange conversions
      # like: mydata.seq
      rawAddSon(s.typ, newTypeS(tyEmpty, c))
      s.ast = a
      inc c.InGenericContext
      var body = semTypeNode(c, a.sons[2], nil)
      dec c.InGenericContext
      if body != nil:
        body.sym = s
        body.size = -1 # could not be computed properly
      s.typ.sons[sonsLen(s.typ) - 1] = body
      popOwner()
      closeScope(c.tab)
    elif a.sons[2].kind != nkEmpty: 
      # process the type's body:
      pushOwner(s)
      var t = semTypeNode(c, a.sons[2], s.typ)
      if s.typ == nil: 
        s.typ = t
      elif t != s.typ: 
        # this can happen for e.g. tcan_alias_specialised_generic:
        assignType(s.typ, t)
        #debug s.typ
      s.ast = a
      popOwner()

proc typeSectionFinalPass(c: PContext, n: PNode) = 
  for i in countup(0, sonsLen(n) - 1): 
    var a = n.sons[i]
    if a.kind == nkCommentStmt: continue 
    if a.sons[0].kind != nkSym: IllFormedAst(a)
    var s = a.sons[0].sym
    # compute the type's size and check for illegal recursions:
    if a.sons[1].kind == nkEmpty: 
      if a.sons[2].kind in {nkSym, nkIdent, nkAccQuoted}:
        # type aliases are hard:
        #MessageOut('for type ' + typeToString(s.typ));
        var t = semTypeNode(c, a.sons[2], nil)
        if t.kind in {tyObject, tyEnum}: 
          assignType(s.typ, t)
          s.typ.id = t.id     # same id
      checkConstructedType(s.info, s.typ)
    let aa = a.sons[2]
    if aa.kind in {nkRefTy, nkPtrTy} and aa.len == 1 and
       aa.sons[0].kind == nkObjectTy:
      # give anonymous object a dummy symbol:
      assert s.typ.sons[0].sym == nil
      s.typ.sons[0].sym = newSym(skType, getIdent(s.name.s & ":ObjectType"), 
                                 getCurrOwner(), s.info)

proc SemTypeSection(c: PContext, n: PNode): PNode =
  typeSectionLeftSidePass(c, n)
  typeSectionRightSidePass(c, n)
  typeSectionFinalPass(c, n)
  result = n

proc semParamList(c: PContext, n, genericParams: PNode, s: PSym) =
  s.typ = semProcTypeNode(c, n, genericParams, nil, s.kind)

proc addParams(c: PContext, n: PNode, kind: TSymKind) = 
  for i in countup(1, sonsLen(n)-1): 
    if n.sons[i].kind == nkSym: addParamOrResult(c, n.sons[i].sym, kind)
    else: illFormedAst(n)

proc semBorrow(c: PContext, n: PNode, s: PSym) = 
  # search for the correct alias:
  var b = SearchForBorrowProc(c, s, c.tab.tos - 2)
  if b != nil: 
    # store the alias:
    n.sons[bodyPos] = newSymNode(b)
  else:
    LocalError(n.info, errNoSymbolToBorrowFromFound) 
  
proc addResult(c: PContext, t: PType, info: TLineInfo, owner: TSymKind) = 
  if t != nil: 
    var s = newSym(skResult, getIdent"result", getCurrOwner(), info)
    s.typ = t
    incl(s.flags, sfUsed)
    addParamOrResult(c, s, owner)
    c.p.resultSym = s

proc addResultNode(c: PContext, n: PNode) = 
  if c.p.resultSym != nil: addSon(n, newSymNode(c.p.resultSym))

proc copyExcept(n: PNode, i: int): PNode =
  result = copyNode(n)
  for j in 0.. <n.len:
    if j != i: result.add(n.sons[j])

proc lookupMacro(c: PContext, n: PNode): PSym =
  if n.kind == nkSym:
    result = n.sym
    if result.kind notin {skMacro, skTemplate}: result = nil
  else:
    result = SymtabGet(c.Tab, considerAcc(n), {skMacro, skTemplate})

proc semProcAnnotation(c: PContext, prc: PNode): PNode =
  var n = prc.sons[pragmasPos]
  if n == nil or n.kind == nkEmpty: return
  for i in countup(0, <n.len):
    var it = n.sons[i]
    var key = if it.kind == nkExprColonExpr: it.sons[0] else: it
    let m = lookupMacro(c, key)
    if m == nil: continue
    # we transform ``proc p {.m, rest.}`` into ``m(do: proc p {.rest.})`` and
    # let the semantic checker deal with it:
    var x = newNodeI(nkCall, n.info)
    x.add(newSymNode(m))
    prc.sons[pragmasPos] = copyExcept(n, i)
    if it.kind == nkExprColonExpr:
      # pass pragma argument to the macro too:
      x.add(it.sons[1])
    x.add(newProcNode(nkDo, prc.info, prc))
    # recursion assures that this works for multiple macro annotations too:
    return semStmt(c, x)

proc semLambda(c: PContext, n: PNode, flags: TExprFlags): PNode =
  result = semProcAnnotation(c, n)
  if result != nil: return result
  result = n
  checkSonsLen(n, bodyPos + 1)
  var s: PSym
  if n[namePos].kind != nkSym:
    s = newSym(skProc, idAnon, getCurrOwner(), n.info)
    s.ast = n
    n.sons[namePos] = newSymNode(s)
  else:
    s = n[namePos].sym
  pushOwner(s)
  openScope(c.tab)
  if n.sons[genericParamsPos].kind != nkEmpty:
    illFormedAst(n)           # process parameters:
  if n.sons[paramsPos].kind != nkEmpty: 
    semParamList(c, n.sons[ParamsPos], nil, s)
    ParamsTypeCheck(c, s.typ)
  else:
    s.typ = newTypeS(tyProc, c)
    rawAddSon(s.typ, nil)
  if n.sons[pragmasPos].kind != nkEmpty:
    pragma(c, s, n.sons[pragmasPos], lambdaPragmas)
  s.options = gOptions
  if n.sons[bodyPos].kind != nkEmpty:
    if sfImportc in s.flags:
      LocalError(n.sons[bodyPos].info, errImplOfXNotAllowed, s.name.s)
    #if efDetermineType notin flags:
    # XXX not good enough; see tnamedparamanonproc.nim
    pushProcCon(c, s)
    addResult(c, s.typ.sons[0], n.info, skProc)
    let semBody = hloBody(c, semProcBody(c, n.sons[bodyPos]))
    n.sons[bodyPos] = transformBody(c.module, semBody, s)
    addResultNode(c, n)
    popProcCon(c)
    sideEffectsCheck(c, s)
  else:
    LocalError(n.info, errImplOfXexpected, s.name.s)
  closeScope(c.tab)           # close scope for parameters
  popOwner()
  result.typ = s.typ

proc activate(c: PContext, n: PNode) =
  # XXX: This proc is part of my plan for getting rid of
  # forward declarations. stay tuned.
  when false:
    # well for now it breaks code ...
    case n.kind
    of nkLambdaKinds:
      discard semLambda(c, n, {})
    of nkCallKinds:
      for i in 1 .. <n.len: activate(c, n[i])
    else:
      nil

proc instantiateDestructor*(c: PContext, typ: PType): bool

proc doDestructorStuff(c: PContext, s: PSym, n: PNode) =
  let t = s.typ.sons[1].skipTypes({tyVar})
  t.destructor = s
  # automatically insert calls to base classes' destructors
  if n.sons[bodyPos].kind != nkEmpty:
    for i in countup(0, t.sonsLen - 1):
      # when inheriting directly from object
      # there will be a single nil son
      if t.sons[i] == nil: continue
      if instantiateDestructor(c, t.sons[i]):
        n.sons[bodyPos].addSon(newNode(nkCall, t.sym.info, @[
            useSym(t.sons[i].destructor),
            n.sons[paramsPos][1][0]]))

proc maybeAddResult(c: PContext, s: PSym, n: PNode) =
  if s.typ.sons[0] != nil and
      (s.kind != skIterator or s.typ.callConv == ccClosure):
    addResult(c, s.typ.sons[0], n.info, s.kind)
    addResultNode(c, n)

proc semProcAux(c: PContext, n: PNode, kind: TSymKind, 
                validPragmas: TSpecialWords): PNode = 
  result = semProcAnnotation(c, n)
  if result != nil: return result
  result = n
  checkSonsLen(n, bodyPos + 1)
  var s = semIdentDef(c, n.sons[0], kind)
  n.sons[namePos] = newSymNode(s)
  s.ast = n
  pushOwner(s)
  openScope(c.tab)
  var gp: PNode
  if n.sons[genericParamsPos].kind != nkEmpty: 
    n.sons[genericParamsPos] = semGenericParamList(c, n.sons[genericParamsPos])
    gp = n.sons[genericParamsPos]
  else: 
    gp = newNodeI(nkGenericParams, n.info)
  # process parameters:
  if n.sons[paramsPos].kind != nkEmpty:
    semParamList(c, n.sons[ParamsPos], gp, s)
    if sonsLen(gp) > 0: 
      if n.sons[genericParamsPos].kind == nkEmpty:
        # we have a list of implicit type parameters:
        n.sons[genericParamsPos] = gp
        # check for semantics again:
        # semParamList(c, n.sons[ParamsPos], nil, s)
  else:
    s.typ = newTypeS(tyProc, c)
    rawAddSon(s.typ, nil)
  if n.sons[patternPos].kind != nkEmpty:
    n.sons[patternPos] = semPattern(c, n.sons[patternPos])
  if s.kind == skIterator: s.typ.flags.incl(tfIterator)
  
  var proto = SearchForProc(c, s, c.tab.tos-2) # -2 because we have a scope
                                               # open for parameters
  if proto == nil: 
    s.typ.callConv = lastOptionEntry(c).defaultCC
    # add it here, so that recursive procs are possible:
    # -2 because we have a scope open for parameters
    if sfGenSym in s.flags: nil
    elif kind in OverloadableSyms: 
      addInterfaceOverloadableSymAt(c, s, c.tab.tos - 2)
    else: 
      addInterfaceDeclAt(c, s, c.tab.tos - 2)
    if n.sons[pragmasPos].kind != nkEmpty:
      pragma(c, s, n.sons[pragmasPos], validPragmas)
    else:
      implictPragmas(c, s, n, validPragmas)
  else: 
    if n.sons[pragmasPos].kind != nkEmpty: 
      LocalError(n.sons[pragmasPos].info, errPragmaOnlyInHeaderOfProc)
    if sfForward notin proto.flags: 
      WrongRedefinition(n.info, proto.name.s)
    excl(proto.flags, sfForward)
    closeScope(c.tab)         # close scope with wrong parameter symbols
    openScope(c.tab)          # open scope for old (correct) parameter symbols
    if proto.ast.sons[genericParamsPos].kind != nkEmpty: 
      addGenericParamListToScope(c, proto.ast.sons[genericParamsPos])
    addParams(c, proto.typ.n, proto.kind)
    proto.info = s.info       # more accurate line information
    s.typ = proto.typ
    s = proto
    n.sons[genericParamsPos] = proto.ast.sons[genericParamsPos]
    n.sons[paramsPos] = proto.ast.sons[paramsPos]
    n.sons[pragmasPos] = proto.ast.sons[pragmasPos]
    if n.sons[namePos].kind != nkSym: InternalError(n.info, "semProcAux")
    n.sons[namePos].sym = proto
    if importantComments() and not isNil(proto.ast.comment):
      n.comment = proto.ast.comment
    proto.ast = n             # needed for code generation
    popOwner()
    pushOwner(s)
  s.options = gOptions
  if sfDestructor in s.flags: doDestructorStuff(c, s, n)
  if n.sons[bodyPos].kind != nkEmpty: 
    # for DLL generation it is annoying to check for sfImportc!
    if sfBorrow in s.flags: 
      LocalError(n.sons[bodyPos].info, errImplOfXNotAllowed, s.name.s)
    if n.sons[genericParamsPos].kind == nkEmpty: 
      ParamsTypeCheck(c, s.typ)
      pushProcCon(c, s)
      maybeAddResult(c, s, n)
      if sfImportc notin s.flags:
        # no semantic checking for importc:
        let semBody = hloBody(c, semProcBody(c, n.sons[bodyPos]))
        # unfortunately we cannot skip this step when in 'system.compiles'
        # context as it may even be evaluated in 'system.compiles':
        n.sons[bodyPos] = transformBody(c.module, semBody, s)
      popProcCon(c)
    else: 
      if s.typ.sons[0] != nil and kind != skIterator:
        addDecl(c, newSym(skUnknown, getIdent"result", nil, n.info))
      var toBind = initIntSet()
      n.sons[bodyPos] = semGenericStmtScope(c, n.sons[bodyPos], {}, toBind)
      fixupInstantiatedSymbols(c, s)
    if sfImportc in s.flags: 
      # so we just ignore the body after semantic checking for importc:
      n.sons[bodyPos] = ast.emptyNode
  else:
    if proto != nil: LocalError(n.info, errImplOfXexpected, proto.name.s)
    if {sfImportc, sfBorrow} * s.flags == {} and s.magic == mNone: 
      incl(s.flags, sfForward)
    elif sfBorrow in s.flags: semBorrow(c, n, s)
  sideEffectsCheck(c, s)
  closeScope(c.tab)           # close scope for parameters
  popOwner()
  if n.sons[patternPos].kind != nkEmpty:
    c.patterns.add(s)
  
proc semIterator(c: PContext, n: PNode): PNode =
  result = semProcAux(c, n, skIterator, iteratorPragmas)
  var s = result.sons[namePos].sym
  var t = s.typ
  if t.sons[0] == nil and s.typ.callConv != ccClosure:
    LocalError(n.info, errXNeedsReturnType, "iterator")
  # iterators are either 'inline' or 'closure'; for backwards compatibility,
  # we require first class iterators to be marked with 'closure' explicitly
  # -- at least for 0.9.2.
  if s.typ.callConv == ccClosure:
    incl(s.typ.flags, tfCapturesEnv)
  when false:
    if s.typ.callConv != ccInline: 
      s.typ.callConv = ccClosure
      # and they always at least use the 'env' for the state field:
      incl(s.typ.flags, tfCapturesEnv)
  if n.sons[bodyPos].kind == nkEmpty and s.magic == mNone:
    LocalError(n.info, errImplOfXexpected, s.name.s)
  
proc semProc(c: PContext, n: PNode): PNode = 
  result = semProcAux(c, n, skProc, procPragmas)

proc hasObjParam(s: PSym): bool =
  var t = s.typ
  for col in countup(1, sonsLen(t)-1):
    if skipTypes(t.sons[col], skipPtrs).kind == tyObject:
      return true

proc finishMethod(c: PContext, s: PSym) =
  if hasObjParam(s):
    methodDef(s, false)

proc semMethod(c: PContext, n: PNode): PNode = 
  if not isTopLevel(c): LocalError(n.info, errXOnlyAtModuleScope, "method")
  result = semProcAux(c, n, skMethod, methodPragmas)
  
  var s = result.sons[namePos].sym
  if not isGenericRoutine(s):
    if hasObjParam(s):
      methodDef(s, false)
    else:
      LocalError(n.info, errXNeedsParamObjectType, "method")

proc semConverterDef(c: PContext, n: PNode): PNode = 
  if not isTopLevel(c): LocalError(n.info, errXOnlyAtModuleScope, "converter")
  checkSonsLen(n, bodyPos + 1)
  result = semProcAux(c, n, skConverter, converterPragmas)
  var s = result.sons[namePos].sym
  var t = s.typ
  if t.sons[0] == nil: LocalError(n.info, errXNeedsReturnType, "converter")
  if sonsLen(t) != 2: LocalError(n.info, errXRequiresOneArgument, "converter")
  addConverter(c, s)

proc semMacroDef(c: PContext, n: PNode): PNode = 
  checkSonsLen(n, bodyPos + 1)
  result = semProcAux(c, n, skMacro, macroPragmas)
  var s = result.sons[namePos].sym
  var t = s.typ
  if t.sons[0] == nil: LocalError(n.info, errXNeedsReturnType, "macro")
  if n.sons[bodyPos].kind == nkEmpty:
    LocalError(n.info, errImplOfXexpected, s.name.s)
  
proc evalInclude(c: PContext, n: PNode): PNode = 
  result = newNodeI(nkStmtList, n.info)
  addSon(result, n)
  for i in countup(0, sonsLen(n) - 1): 
    var f = checkModuleName(n.sons[i])
    if f != InvalidFileIDX:
      if ContainsOrIncl(c.includedFiles, f): 
        LocalError(n.info, errRecursiveDependencyX, f.toFilename)
      else:
        addSon(result, semStmt(c, gIncludeFile(c.module, f)))
        Excl(c.includedFiles, f)
  
proc setLine(n: PNode, info: TLineInfo) =
  for i in 0 .. <safeLen(n): setLine(n.sons[i], info)
  n.info = info
  
proc semPragmaBlock(c: PContext, n: PNode): PNode =
  let pragmaList = n.sons[0]
  pragma(c, nil, pragmaList, exprPragmas)
  result = semStmt(c, n.sons[1])
  for i in 0 .. <pragmaList.len:
    if whichPragma(pragmaList.sons[i]) == wLine:
      setLine(result, pragmaList.sons[i].info)

proc semStaticStmt(c: PContext, n: PNode): PNode =
  let a = semStmt(c, n.sons[0])
  result = evalStaticExpr(c.module, a)
  if result.isNil:
    LocalError(n.info, errCannotInterpretNodeX, renderTree(n))
  elif result.kind == nkEmpty:
    result = newNodeI(nkDiscardStmt, n.info, 1)
    result.sons[0] = emptyNode

# special marker values that indicates that we are
# 1) AnalyzingDestructor: currently analyzing the type for destructor 
# generation (needed for recursive types)
# 2) DestructorIsTrivial: completed the analysis before and determined
# that the type has a trivial destructor
var AnalyzingDestructor, DestructorIsTrivial: PSym
new(AnalyzingDestructor)
new(DestructorIsTrivial)

var
  destructorName = getIdent"destroy_"
  destructorParam = getIdent"this_"
  destructorPragma = newIdentNode(getIdent"destructor", UnknownLineInfo())
  rangeDestructorProc*: PSym

proc destroyField(c: PContext, field: PSym, holder: PNode): PNode =
  if instantiateDestructor(c, field.typ):
    result = newNode(nkCall, field.info, @[
      useSym(field.typ.destructor),
      newNode(nkDotExpr, field.info, @[holder, useSym(field)])])

proc destroyCase(c: PContext, n: PNode, holder: PNode): PNode =
  var nonTrivialFields = 0
  result = newNode(nkCaseStmt, n.info, @[])
  # case x.kind
  result.addSon(newNode(nkDotExpr, n.info, @[holder, n.sons[0]]))
  for i in countup(1, n.len - 1):
    # of A, B:
    var caseBranch = newNode(n[i].kind, n[i].info, n[i].sons[0 .. -2])
    let recList = n[i].lastSon
    var destroyRecList = newNode(nkStmtList, n[i].info, @[])
    template addField(f: expr): stmt =
      let stmt = destroyField(c, f, holder)
      if stmt != nil:
        destroyRecList.addSon(stmt)
        inc nonTrivialFields
        
    case recList.kind
    of nkSym:
      addField(recList.sym)
    of nkRecList:
      for j in countup(0, recList.len - 1):
        addField(recList[j].sym)
    else:
      internalAssert false
      
    caseBranch.addSon(destroyRecList)
    result.addSon(caseBranch)
  # maybe no fields were destroyed?
  if nonTrivialFields == 0:
    result = nil
 
proc generateDestructor(c: PContext, t: PType): PNode =
  ## generate a destructor for a user-defined object ot tuple type
  ## returns nil if the destructor turns out to be trivial
  
  template addLine(e: expr): stmt =
    if result == nil: result = newNode(nkStmtList)
    result.addSon(e)

  # XXX: This may be true for some C-imported types such as
  # Tposix_spawnattr
  if t.n == nil or t.n.sons == nil: return
  internalAssert t.n.kind == nkRecList
  let destructedObj = newIdentNode(destructorParam, UnknownLineInfo())
  # call the destructods of all fields
  for s in countup(0, t.n.sons.len - 1):
    case t.n.sons[s].kind
    of nkRecCase:
      let stmt = destroyCase(c, t.n.sons[s], destructedObj)
      if stmt != nil: addLine(stmt)
    of nkSym:
      let stmt = destroyField(c, t.n.sons[s].sym, destructedObj)
      if stmt != nil: addLine(stmt)
    else:
      internalAssert false

  # base classes' destructors will be automatically called by
  # semProcAux for both auto-generated and user-defined destructors

proc instantiateDestructor*(c: PContext, typ: PType): bool =
  # returns true if the type already had a user-defined
  # destructor or if the compiler generated a default
  # member-wise one
  var t = skipTypes(typ, {tyConst, tyMutable})
  
  if t.destructor != nil:
    # XXX: This is not entirely correct for recursive types, but we need
    # it temporarily to hide the "destroy is alrady defined" problem
    return t.destructor notin [AnalyzingDestructor, DestructorIsTrivial]
  
  case t.kind
  of tySequence, tyArray, tyArrayConstr, tyOpenArray, tyVarargs:
    if instantiateDestructor(c, t.sons[0]):
      if rangeDestructorProc == nil:
        rangeDestructorProc = SymtabGet(c.tab, getIdent"nimDestroyRange")
      t.destructor = rangeDestructorProc
      return true
    else:
      return false
  of tyTuple, tyObject:
    t.destructor = AnalyzingDestructor
    let generated = generateDestructor(c, t)
    if generated != nil:
      internalAssert t.sym != nil
      var i = t.sym.info
      let fullDef = newNode(nkProcDef, i, @[
        newIdentNode(destructorName, i),
        emptyNode,
        emptyNode,
        newNode(nkFormalParams, i, @[
          emptyNode,
          newNode(nkIdentDefs, i, @[
            newIdentNode(destructorParam, i),
            useSym(t.sym),
            emptyNode]),
          ]),
        newNode(nkPragma, i, @[destructorPragma]),
        emptyNode,
        generated
        ])
      discard semProc(c, fullDef)
      internalAssert t.destructor != nil
      return true
    else:
      t.destructor = DestructorIsTrivial
      return false
  else:
    return false

proc insertDestructors(c: PContext,
                       varSection: PNode): tuple[outer, inner: PNode] =
  # Accepts a var or let section.
  #
  # When a var section has variables with destructors
  # the var section is split up and finally blocks are inserted
  # immediately after all "destructable" vars
  #
  # In case there were no destrucable variables, the proc returns
  # (nil, nil) and the enclosing stmt-list requires no modifications.
  #
  # Otherwise, after the try blocks are created, the rest of the enclosing
  # stmt-list should be inserted in the most `inner` such block (corresponding
  # to the last variable).
  #
  # `outer` is a statement list that should replace the original var section.
  # It will include the new truncated var section followed by the outermost
  # try block.
  let totalVars = varSection.sonsLen
  for j in countup(0, totalVars - 1):
    let
      varId = varSection[j][0]
      varTyp = varId.sym.typ
      info = varId.info

    if varTyp != nil and instantiateDestructor(c, varTyp) and 
        sfGlobal notin varId.sym.flags:
      var tryStmt = newNodeI(nkTryStmt, info)

      if j < totalVars - 1:
        var remainingVars = newNodeI(varSection.kind, info)
        remainingVars.sons = varSection.sons[(j+1)..(-1)]
        let (outer, inner) = insertDestructors(c, remainingVars)
        if outer != nil:
          tryStmt.addSon(outer)
          result.inner = inner
        else:
          result.inner = newNodeI(nkStmtList, info)
          result.inner.addSon(remainingVars)
          tryStmt.addSon(result.inner)
      else:
        result.inner = newNodeI(nkStmtList, info)
        tryStmt.addSon(result.inner)

      tryStmt.addSon(
        newNode(nkFinally, info, @[
          semStmt(c, newNode(nkCall, info, @[
            useSym(varTyp.destructor),
            useSym(varId.sym)]))]))

      result.outer = newNodeI(nkStmtList, info)
      varSection.sons.setLen(j+1)
      result.outer.addSon(varSection)
      result.outer.addSon(tryStmt)

      return

proc ImplicitelyDiscardable(n: PNode): bool =
  result = isCallExpr(n) and n.sons[0].kind == nkSym and 
           sfDiscardable in n.sons[0].sym.flags

proc semStmtList(c: PContext, n: PNode): PNode =
  # these must be last statements in a block:
  const
    LastBlockStmts = {nkRaiseStmt, nkReturnStmt, nkBreakStmt, nkContinueStmt}
  result = n
  var length = sonsLen(n)
  for i in countup(0, length - 1):
    case n.sons[i].kind
    of nkFinally, nkExceptBranch:
      # stand-alone finally and except blocks are
      # transformed into regular try blocks:
      #
      # var f = fopen("somefile") | var f = fopen("somefile")
      # finally: fclose(f)        | try:
      # ...                       |   ...
      #                           | finally:
      #                           |   fclose(f)
      var tryStmt = newNodeI(nkTryStmt, n.sons[i].info)
      var body = newNodeI(nkStmtList, n.sons[i].info)
      if i < n.sonsLen - 1:
        body.sons = n.sons[(i+1)..(-1)]
      tryStmt.addSon(body)
      tryStmt.addSon(n.sons[i])
      n.sons[i] = semTry(c, tryStmt)
      n.sons.setLen(i+1)
      return
    else:
      n.sons[i] = semStmt(c, n.sons[i])
      case n.sons[i].kind
      of nkVarSection, nkLetSection:
        let (outer, inner) = insertDestructors(c, n.sons[i])
        if outer != nil:
          n.sons[i] = outer
          for j in countup(i+1, length-1):
            inner.addSon(SemStmt(c, n.sons[j]))
          n.sons.setLen(i+1)
          return
      of LastBlockStmts: 
        for j in countup(i + 1, length - 1): 
          case n.sons[j].kind
          of nkPragma, nkCommentStmt, nkNilLit, nkEmpty: nil
          else: localError(n.sons[j].info, errStmtInvalidAfterReturn)
      else: nil
  
  # a statement list (s; e) has the type 'e':
  if result.kind == nkStmtList and result.len > 0:
    var lastStmt = lastSon(result)
    if lastStmt.kind != nkNilLit and not ImplicitelyDiscardable(lastStmt):
      result.typ = lastStmt.typ
      #localError(lastStmt.info, errGenerated,
      #  "Last expression must be explicitly returned if it " &
      #  "is discardable or discarded")

proc SemStmt(c: PContext, n: PNode): PNode = 
  # now: simply an alias:
  result = semExprNoType(c, n)

proc semStmtScope(c: PContext, n: PNode): PNode =
  openScope(c.tab)
  result = semStmt(c, n)
  closeScope(c.tab)
