#
#
#           The Nimrod Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from sem.nim

discard """
  hygienic templates: 
  
    template `||` (a, b: expr): expr =
      let aa = a
      (if aa: aa else: b)
    
    var
      a, b: T
      
    a || b || a
    
  Each evaluation context has to be different and we need to perform
  some form of preliminary symbol lookup in template definitions. Hygiene is
  a way to achieve lexical scoping at compile time.
"""

type
  TSymBinding = enum
    spNone, spGenSym, spInject

proc symBinding(n: PNode): TSymBinding =
  for i in countup(0, sonsLen(n) - 1):
    var it = n.sons[i]
    var key = if it.kind == nkExprColonExpr: it.sons[0] else: it
    if key.kind == nkIdent:
      case whichKeyword(key.ident)
      of wGenSym: return spGenSym
      of wInject: return spInject
      else: nil

type
  TSymChoiceRule = enum
    scClosed, scOpen, scForceOpen

proc symChoice(c: PContext, n: PNode, s: PSym, r: TSymChoiceRule): PNode =
  var
    a: PSym
    o: TOverloadIter
  var i = 0
  a = initOverloadIter(o, c, n)
  while a != nil: 
    a = nextOverloadIter(o, c, n)
    inc(i)
    if i > 1: break
  if i <= 1 and r != scForceOpen:
    # XXX this makes more sense but breaks bootstrapping for now:
    # (s.kind notin routineKinds or s.magic != mNone):
    # for instance 'nextTry' is both in tables.nim and astalgo.nim ...
    result = newSymNode(s, n.info)
    markUsed(n, s)
  else:
    # semantic checking requires a type; ``fitNode`` deals with it
    # appropriately
    let kind = if r == scClosed: nkClosedSymChoice else: nkOpenSymChoice
    result = newNodeIT(kind, n.info, newTypeS(tyNone, c))
    a = initOverloadIter(o, c, n)
    while a != nil:
      incl(a.flags, sfUsed)
      addSon(result, newSymNode(a, n.info))
      a = nextOverloadIter(o, c, n)

proc semBindStmt(c: PContext, n: PNode, toBind: var TIntSet): PNode =
  for i in 0 .. < n.len:
    var a = n.sons[i]
    # If 'a' is an overloaded symbol, we used to use the first symbol
    # as a 'witness' and use the fact that subsequent lookups will yield
    # the same symbol!
    # This is however not true anymore for hygienic templates as semantic
    # processing for them changes the symbol table...
    let s = QualifiedLookUp(c, a)
    if s != nil:
      # we need to mark all symbols:
      let sc = symChoice(c, n, s, scClosed)
      if sc.kind == nkSym:
        toBind.incl(sc.sym.id)
      else:
        for x in items(sc): toBind.incl(x.sym.id)
    else:
      illFormedAst(a)
  result = newNodeI(nkEmpty, n.info)
  
proc replaceIdentBySym(n: var PNode, s: PNode) =
  case n.kind
  of nkPostfix: replaceIdentBySym(n.sons[1], s)
  of nkPragmaExpr: replaceIdentBySym(n.sons[0], s)
  of nkIdent, nkAccQuoted, nkSym: n = s
  else: illFormedAst(n)

# This code here is the first pass over a template's body. The same code also
# implements the first pass over a pattern's body:

type
  TBodyKind = enum
    bkTemplate, bkPattern
  TemplCtx {.pure, final.} = object
    c: PContext
    toBind: TIntSet
    bodyKind: TBodyKind
    owner: PSym

proc getIdentNode(c: var TemplCtx, n: PNode): PNode =
  case n.kind
  of nkPostfix: result = getIdentNode(c, n.sons[1])
  of nkPragmaExpr: result = getIdentNode(c, n.sons[0])
  of nkIdent:
    result = n
    let s = QualifiedLookUp(c.c, n, {})
    if s != nil:
      if s.owner == c.owner and s.kind == skParam:
        result = newSymNode(s, n.info)
  of nkAccQuoted, nkSym: result = n
  else:
    illFormedAst(n)
    result = n

proc isTemplParam(c: TemplCtx, n: PNode): bool {.inline.} =
  result = n.kind == nkSym and n.sym.kind == skParam and
           n.sym.owner == c.owner

proc semTemplBody(c: var TemplCtx, n: PNode): PNode

proc openScope(c: var TemplCtx) = openScope(c.c.tab)
proc closeScope(c: var TemplCtx) = closeScope(c.c.tab)

proc semTemplBodyScope(c: var TemplCtx, n: PNode): PNode = 
  openScope(c)
  result = semTemplBody(c, n)
  closeScope(c)

proc newGenSym(kind: TSymKind, n: PNode, c: var TemplCtx): PSym =
  result = newSym(kind, considerAcc(n), c.owner, n.info)
  incl(result.flags, sfGenSym)
  incl(result.flags, sfShadowed)

proc addLocalDecl(c: var TemplCtx, n: var PNode, k: TSymKind) =
  # locals default to 'gensym':
  if n.kind != nkPragmaExpr or symBinding(n.sons[1]) != spInject:
    let ident = getIdentNode(c, n)
    if not isTemplParam(c, ident):
      let local = newGenSym(k, ident, c)
      addPrelimDecl(c.c, local)
      replaceIdentBySym(n, newSymNode(local, n.info))
    else:
      replaceIdentBySym(n, ident)
  else:
    n = semTemplBody(c, n)

proc semRoutineInTemplBody(c: var TemplCtx, n: PNode, k: TSymKind): PNode =
  result = n
  checkSonsLen(n, bodyPos + 1)
  # routines default to 'inject':
  if n.kind notin nkLambdaKinds and symBinding(n.sons[pragmasPos]) == spGenSym:
    let ident = getIdentNode(c, n.sons[namePos])
    if not isTemplParam(c, ident):
      let s = newGenSym(k, ident, c)
      addPrelimDecl(c.c, s)
      n.sons[namePos] = newSymNode(s, n.sons[namePos].info)
    else:
      n.sons[namePos] = ident
  else:
    n.sons[namePos] = semTemplBody(c, n.sons[namePos])
  openScope(c)
  for i in patternPos..bodyPos:
    n.sons[i] = semTemplBody(c, n.sons[i])
  closeScope(c)

proc semPattern(c: PContext, n: PNode): PNode
proc semTemplBody(c: var TemplCtx, n: PNode): PNode = 
  result = n
  case n.kind
  of nkIdent:
    let s = QualifiedLookUp(c.c, n, {})
    if s != nil:
      if s.owner == c.owner and s.kind == skParam:
        incl(s.flags, sfUsed)
        result = newSymNode(s, n.info)
      elif Contains(c.toBind, s.id):
        result = symChoice(c.c, n, s, scClosed)
      elif c.bodyKind == bkPattern:
        result = symChoice(c.c, n, s, scOpen)
      elif s.owner == c.owner and sfGenSym in s.flags:
        # template tmp[T](x: var seq[T]) =
        # var yz: T
        incl(s.flags, sfUsed)
        result = newSymNode(s, n.info)
  of nkBind:
    result = semTemplBody(c, n.sons[0])
  of nkBindStmt:
    result = semBindStmt(c.c, n, c.toBind)
  of nkEmpty, nkSym..nkNilLit:
    nil
  of nkIfStmt: 
    for i in countup(0, sonsLen(n)-1): 
      n.sons[i] = semTemplBodyScope(c, n.sons[i])
  of nkWhileStmt: 
    openScope(c)
    for i in countup(0, sonsLen(n)-1): 
      n.sons[i] = semTemplBody(c, n.sons[i])
    closeScope(c)
  of nkCaseStmt:
    openScope(c)
    n.sons[0] = semTemplBody(c, n.sons[0])
    for i in countup(1, sonsLen(n)-1): 
      var a = n.sons[i]
      checkMinSonsLen(a, 1)
      var L = sonsLen(a)
      for j in countup(0, L-2): 
        a.sons[j] = semTemplBody(c, a.sons[j])
      a.sons[L-1] = semTemplBodyScope(c, a.sons[L-1])
    closeScope(c)
  of nkForStmt, nkParForStmt: 
    var L = sonsLen(n)
    openScope(c)
    n.sons[L-2] = semTemplBody(c, n.sons[L-2])
    for i in countup(0, L - 3):
      addLocalDecl(c, n.sons[i], skForVar)
    n.sons[L-1] = semTemplBody(c, n.sons[L-1])
    closeScope(c)
  of nkBlockStmt, nkBlockExpr, nkBlockType:
    checkSonsLen(n, 2)
    openScope(c)
    if n.sons[0].kind != nkEmpty:
      # labels are always 'gensym'ed:
      let s = newGenSym(skLabel, n.sons[0], c)
      addPrelimDecl(c.c, s)
      n.sons[0] = newSymNode(s, n.sons[0].info)
    n.sons[1] = semTemplBody(c, n.sons[1])
    closeScope(c)
  of nkTryStmt: 
    checkMinSonsLen(n, 2)
    n.sons[0] = semTemplBodyScope(c, n.sons[0])
    for i in countup(1, sonsLen(n)-1): 
      var a = n.sons[i]
      checkMinSonsLen(a, 1)
      var L = sonsLen(a)
      for j in countup(0, L-2): 
        a.sons[j] = semTemplBody(c, a.sons[j])
      a.sons[L-1] = semTemplBodyScope(c, a.sons[L-1])
  of nkVarSection, nkLetSection:
    let symKind = if n.kind == nkLetSection: skLet else: skVar
    for i in countup(0, sonsLen(n) - 1): 
      var a = n.sons[i]
      if a.kind == nkCommentStmt: continue 
      if (a.kind != nkIdentDefs) and (a.kind != nkVarTuple): IllFormedAst(a)
      checkMinSonsLen(a, 3)
      var L = sonsLen(a)
      a.sons[L-2] = semTemplBody(c, a.sons[L-2])
      a.sons[L-1] = semTemplBody(c, a.sons[L-1])
      for j in countup(0, L-3):
        addLocalDecl(c, a.sons[j], symKind)
  of nkConstSection:
    for i in countup(0, sonsLen(n) - 1): 
      var a = n.sons[i]
      if a.kind == nkCommentStmt: continue 
      if (a.kind != nkConstDef): IllFormedAst(a)
      checkSonsLen(a, 3)
      addLocalDecl(c, a.sons[0], skConst)
      a.sons[1] = semTemplBody(c, a.sons[1])
      a.sons[2] = semTemplBody(c, a.sons[2])
  of nkTypeSection: 
    for i in countup(0, sonsLen(n) - 1): 
      var a = n.sons[i]
      if a.kind == nkCommentStmt: continue 
      if (a.kind != nkTypeDef): IllFormedAst(a)
      checkSonsLen(a, 3)
      addLocalDecl(c, a.sons[0], skType)
    for i in countup(0, sonsLen(n) - 1):
      var a = n.sons[i]
      if a.kind == nkCommentStmt: continue 
      if (a.kind != nkTypeDef): IllFormedAst(a)
      checkSonsLen(a, 3)
      if a.sons[1].kind != nkEmpty: 
        openScope(c)
        a.sons[1] = semTemplBody(c, a.sons[1])
        a.sons[2] = semTemplBody(c, a.sons[2])
        closeScope(c)
      else: 
        a.sons[2] = semTemplBody(c, a.sons[2])
  of nkProcDef, nkLambdaKinds:
    result = semRoutineInTemplBody(c, n, skProc)
  of nkMethodDef:
    result = semRoutineInTemplBody(c, n, skMethod)
  of nkIteratorDef:
    result = semRoutineInTemplBody(c, n, skIterator)
  of nkTemplateDef:
    result = semRoutineInTemplBody(c, n, skTemplate)
  of nkMacroDef:
    result = semRoutineInTemplBody(c, n, skMacro)
  of nkConverterDef:
    result = semRoutineInTemplBody(c, n, skConverter)
  else:
    # dotExpr is ambiguous: note that we explicitely allow 'x.TemplateParam',
    # so we use the generic code for nkDotExpr too
    if n.kind == nkDotExpr or n.kind == nkAccQuoted:
      let s = QualifiedLookUp(c.c, n, {})
      if s != nil and Contains(c.toBind, s.id):
        return symChoice(c.c, n, s, scClosed)
    result = n
    for i in countup(0, sonsLen(n) - 1):
      result.sons[i] = semTemplBody(c, n.sons[i])

proc semTemplBodyDirty(c: var TemplCtx, n: PNode): PNode = 
  result = n
  case n.kind
  of nkIdent:
    let s = QualifiedLookUp(c.c, n, {})
    if s != nil:
      if s.owner == c.owner and s.kind == skParam:
        result = newSymNode(s, n.info)
      elif Contains(c.toBind, s.id):
        result = symChoice(c.c, n, s, scClosed)
  of nkBind:
    result = semTemplBodyDirty(c, n.sons[0])
  of nkBindStmt:
    result = semBindStmt(c.c, n, c.toBind)
  of nkEmpty, nkSym..nkNilLit:
    nil
  else:
    # dotExpr is ambiguous: note that we explicitely allow 'x.TemplateParam',
    # so we use the generic code for nkDotExpr too
    if n.kind == nkDotExpr or n.kind == nkAccQuoted:
      let s = QualifiedLookUp(c.c, n, {})
      if s != nil and Contains(c.toBind, s.id):
        return symChoice(c.c, n, s, scClosed)
    result = n
    for i in countup(0, sonsLen(n) - 1):
      result.sons[i] = semTemplBodyDirty(c, n.sons[i])
  
proc transformToExpr(n: PNode): PNode = 
  var realStmt: int
  result = n
  case n.kind
  of nkStmtList: 
    realStmt = - 1
    for i in countup(0, sonsLen(n) - 1): 
      case n.sons[i].kind
      of nkCommentStmt, nkEmpty, nkNilLit: 
        nil
      else: 
        if realStmt == - 1: realStmt = i
        else: realStmt = - 2
    if realStmt >= 0: result = transformToExpr(n.sons[realStmt])
    else: n.kind = nkStmtListExpr
  of nkBlockStmt: 
    n.kind = nkBlockExpr
    #nkIfStmt: n.kind = nkIfExpr // this is not correct!
  else:
    nil

proc semTemplateDef(c: PContext, n: PNode): PNode = 
  var s: PSym
  if c.p.owner.kind == skModule: 
    s = semIdentVis(c, skTemplate, n.sons[0], {sfExported})
    incl(s.flags, sfGlobal)
  else:
    s = semIdentVis(c, skTemplate, n.sons[0], {})
  # check parameter list:
  pushOwner(s)
  openScope(c.tab)
  n.sons[namePos] = newSymNode(s, n.sons[namePos].info)
  if n.sons[pragmasPos].kind != nkEmpty:
    pragma(c, s, n.sons[pragmasPos], templatePragmas)

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
    # no explicit return type? -> use tyStmt
    if n.sons[paramsPos].sons[0].kind == nkEmpty:
      # use ``stmt`` as implicit result type
      s.typ.sons[0] = newTypeS(tyStmt, c)
      s.typ.n.sons[0] = newNodeIT(nkType, n.info, s.typ.sons[0])
  else:
    s.typ = newTypeS(tyProc, c)
    # XXX why do we need tyStmt as a return type again?
    s.typ.n = newNodeI(nkFormalParams, n.info)
    rawAddSon(s.typ, newTypeS(tyStmt, c))
    addSon(s.typ.n, newNodeIT(nkType, n.info, s.typ.sons[0]))
  if n.sons[patternPos].kind != nkEmpty:
    n.sons[patternPos] = semPattern(c, n.sons[patternPos])
    c.patterns.add(s)

  var ctx: TemplCtx
  ctx.toBind = initIntSet()
  ctx.c = c
  ctx.owner = s
  ctx.bodyKind = bkTemplate
  if sfDirty in s.flags:
    n.sons[bodyPos] = semTemplBodyDirty(ctx, n.sons[bodyPos])
  else:
    n.sons[bodyPos] = semTemplBody(ctx, n.sons[bodyPos])
  if s.typ.sons[0].kind notin {tyStmt, tyTypeDesc}:
    n.sons[bodyPos] = transformToExpr(n.sons[bodyPos]) 
    # only parameters are resolved, no type checking is performed
  closeScope(c.tab)
  popOwner()
  s.ast = n
  result = n
  if n.sons[bodyPos].kind == nkEmpty: 
    LocalError(n.info, errImplOfXexpected, s.name.s)
  let curScope = c.tab.tos - 1
  var proto = SearchForProc(c, s, curScope)
  if proto == nil:
    addInterfaceOverloadableSymAt(c, s, curScope)
  else:
    SymTabReplace(c.tab.stack[curScope], proto, s)

proc semPattern(c: PContext, n: PNode): PNode =
  # not much to do here: We don't replace operators ``$``, ``*``, ``+``,
  # ``|``, ``~`` as meta operators and strip the leading ``\`` of all
  # operators.
  openScope(c.tab)
  var ctx: TemplCtx
  ctx.toBind = initIntSet()
  ctx.c = c
  ctx.owner = getCurrOwner()
  ctx.bodyKind = bkPattern
  result = semTemplBody(ctx, n)
  if result.kind in {nkStmtList, nkStmtListExpr} and result.len == 1:
    result = result.sons[0]
  closeScope(c.tab)
