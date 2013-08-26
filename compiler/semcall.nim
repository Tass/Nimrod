#
#
#           The Nimrod Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements semantic checking for calls. 
# included from sem.nim

proc sameMethodDispatcher(a, b: PSym): bool =
  result = false
  if a.kind == skMethod and b.kind == skMethod: 
    var aa = lastSon(a.ast)
    var bb = lastSon(b.ast)
    if aa.kind == nkSym and bb.kind == nkSym:
      if aa.sym == bb.sym: 
        result = true
    else:
      nil
      # generics have no dispatcher yet, so we need to compare the method
      # names; however, the names are equal anyway because otherwise we
      # wouldn't even consider them to be overloaded. But even this does
      # not work reliably! See tmultim6 for an example:
      # method collide[T](a: TThing, b: TUnit[T]) is instantiated and not
      # method collide[T](a: TUnit[T], b: TThing)! This means we need to
      # *instantiate* every candidate! However, we don't keep more than 2-3
      # candidated around so we cannot implement that for now. So in order
      # to avoid subtle problems, the call remains ambiguous and needs to
      # be disambiguated by the programmer; this way the right generic is
      # instantiated.
  
proc determineType(c: PContext, s: PSym)

proc
  pickBestCandidate(c: PContext, headSymbol: PNode,
                    n, orig: PNode,
                    initialBinding: PNode,
                    filter: TSymKinds,
                    best, alt: var TCandidate,
                    errors: var seq[string]) =
  var o: TOverloadIter
  var sym = initOverloadIter(o, c, headSymbol)
  var symScope = o.lastOverloadScope

  var z: TCandidate
  
  if sym == nil: return
  initCandidate(best, sym, initialBinding, symScope)
  initCandidate(alt, sym, initialBinding, symScope)
  best.state = csNoMatch
  
  while sym != nil:
    if sym.kind in filter:
      determineType(c, sym)
      initCandidate(z, sym, initialBinding, o.lastOverloadScope)
      z.calleeSym = sym
      matches(c, n, orig, z)
      if errors != nil:
        errors.safeAdd(getProcHeader(sym))
        if z.errors != nil:
          for err in z.errors:
            errors[errors.len - 1].add("\n  " & err)
      if z.state == csMatch:
        # little hack so that iterators are preferred over everything else:
        if sym.kind == skIterator: inc(z.exactMatches, 200)
        case best.state
        of csEmpty, csNoMatch: best = z
        of csMatch:
          var cmp = cmpCandidates(best, z)
          if cmp < 0: best = z   # x is better than the best so far
          elif cmp == 0: alt = z # x is as good as the best so far
          else: nil
    sym = nextOverloadIter(o, c, headSymbol)

proc NotFoundError*(c: PContext, n: PNode, errors: seq[string]) =
  # Gives a detailed error message; this is separated from semOverloadedCall,
  # as semOverlodedCall is already pretty slow (and we need this information
  # only in case of an error).
  if c.InCompilesContext > 0: 
    # fail fast:
    GlobalError(n.info, errTypeMismatch, "")
  var result = msgKindToString(errTypeMismatch)
  add(result, describeArgs(c, n, 1 + ord(nfDelegate in n.flags)))
  add(result, ')')
  
  var candidates = ""
  for err in errors:
    add(candidates, err)
    add(candidates, "\n")

  if candidates != "":
    add(result, "\n" & msgKindToString(errButExpected) & "\n" & candidates)

  LocalError(n.Info, errGenerated, result)

proc resolveOverloads(c: PContext, n, orig: PNode,
                      filter: TSymKinds): TCandidate =
  var initialBinding: PNode
  var alt: TCandidate
  var f = n.sons[0]
  if f.kind == nkBracketExpr:
    # fill in the bindings:
    initialBinding = f
    f = f.sons[0]
  else:
    initialBinding = nil

  var errors: seq[string]

  template pickBest(headSymbol: expr) =
    pickBestCandidate(c, headSymbol, n, orig, initialBinding,
                      filter, result, alt, errors)

  pickBest(f)

  let overloadsState = result.state
  if overloadsState != csMatch:
    if nfDelegate in n.flags:
      InternalAssert f.kind == nkIdent
      let calleeName = newStrNode(nkStrLit, f.ident.s)
      calleeName.info = n.info

      let callOp = newIdentNode(idDelegator, n.info)
      n.sons[0..0] = [callOp, calleeName]
      orig.sons[0..0] = [callOp, calleeName]
     
      pickBest(callOp)

    if overloadsState == csEmpty and result.state == csEmpty:
      LocalError(n.info, errUndeclaredIdentifier, considerAcc(f).s)
      return
    elif result.state != csMatch:
      if nfExprCall in n.flags:
        if c.inCompilesContext > 0 or gErrorCounter == 0:
          LocalError(n.info, errExprXCannotBeCalled,
                     renderTree(n, {renderNoComments}))
      else:
        errors = @[]
        pickBest(f)
        NotFoundError(c, n, errors)
      return

  if alt.state == csMatch and cmpCandidates(result, alt) == 0 and
      not sameMethodDispatcher(result.calleeSym, alt.calleeSym):
    InternalAssert result.state == csMatch
    #writeMatches(result)
    #writeMatches(alt)
    if c.inCompilesContext > 0: 
      # quick error message for performance of 'compiles' built-in:
      GlobalError(n.Info, errGenerated, "ambiguous call")
    elif gErrorCounter == 0:
      # don't cascade errors
      var args = "("
      for i in countup(1, sonsLen(n) - 1):
        if i > 1: add(args, ", ")
        add(args, typeToString(n.sons[i].typ))
      add(args, ")")

      LocalError(n.Info, errGenerated, msgKindToString(errAmbiguousCallXYZ) % [
        getProcHeader(result.calleeSym), getProcHeader(alt.calleeSym),
        args])


proc instGenericConvertersArg*(c: PContext, a: PNode, x: TCandidate) =
  if a.kind == nkHiddenCallConv and a.sons[0].kind == nkSym and
      isGenericRoutine(a.sons[0].sym):
    let finalCallee = generateInstance(c, a.sons[0].sym, x.bindings, a.info)
    a.sons[0].sym = finalCallee
    a.sons[0].typ = finalCallee.typ
    #a.typ = finalCallee.typ.sons[0]

proc instGenericConvertersSons*(c: PContext, n: PNode, x: TCandidate) =
  assert n.kind in nkCallKinds
  if x.genericConverter:
    for i in 1 .. <n.len:
      instGenericConvertersArg(c, n.sons[i], x)

proc IndexTypesMatch(c: PContext, f, a: PType, arg: PNode): PNode = 
  var m: TCandidate
  initCandidate(m, f)
  result = paramTypesMatch(c, m, f, a, arg, nil)
  if m.genericConverter and result != nil:
    instGenericConvertersArg(c, result, m)

proc ConvertTo*(c: PContext, f: PType, n: PNode): PNode = 
  var m: TCandidate
  initCandidate(m, f)
  result = paramTypesMatch(c, m, f, n.typ, n, nil)
  if m.genericConverter and result != nil:
    instGenericConvertersArg(c, result, m)

proc semResolvedCall(c: PContext, n: PNode, x: TCandidate): PNode =
  assert x.state == csMatch
  var finalCallee = x.calleeSym
  markUsed(n.sons[0], finalCallee)
  if finalCallee.ast == nil:
    internalError(n.info, "calleeSym.ast is nil") # XXX: remove this check!
  if finalCallee.ast.sons[genericParamsPos].kind != nkEmpty:
    # a generic proc!
    if not x.proxyMatch:
      finalCallee = generateInstance(c, x.calleeSym, x.bindings, n.info)
    else:
      result = x.call
      result.sons[0] = newSymNode(finalCallee, result.sons[0].info)
      result.typ = finalCallee.typ.sons[0]
      if ContainsGenericType(result.typ): result.typ = errorType(c)
      return
  result = x.call
  instGenericConvertersSons(c, result, x)
  result.sons[0] = newSymNode(finalCallee, result.sons[0].info)
  result.typ = finalCallee.typ.sons[0]

proc semOverloadedCall(c: PContext, n, nOrig: PNode,
                       filter: TSymKinds): PNode =
  var r = resolveOverloads(c, n, nOrig, filter)
  if r.state == csMatch: result = semResolvedCall(c, n, r)
  else: result = errorNode(c, n)
    
proc explicitGenericInstError(n: PNode): PNode =
  LocalError(n.info, errCannotInstantiateX, renderTree(n))
  result = n

proc explicitGenericSym(c: PContext, n: PNode, s: PSym): PNode =
  var x: TCandidate
  initCandidate(x, s, n)
  var newInst = generateInstance(c, s, x.bindings, n.info)
  markUsed(n, s)
  result = newSymNode(newInst, n.info)

proc explicitGenericInstantiation(c: PContext, n: PNode, s: PSym): PNode = 
  assert n.kind == nkBracketExpr
  for i in 1..sonsLen(n)-1:
    n.sons[i].typ = semTypeNode(c, n.sons[i], nil)
  var s = s
  var a = n.sons[0]
  if a.kind == nkSym:
    # common case; check the only candidate has the right
    # number of generic type parameters:
    if safeLen(s.ast.sons[genericParamsPos]) != n.len-1:
      let expected = safeLen(s.ast.sons[genericParamsPos])
      LocalError(n.info, errGenerated, "cannot instantiate: " & renderTree(n) &
         "; got " & $(n.len-1) & " type(s) but expected " & $expected)
      return n
    result = explicitGenericSym(c, n, s)
  elif a.kind in {nkClosedSymChoice, nkOpenSymChoice}:
    # choose the generic proc with the proper number of type parameters.
    # XXX I think this could be improved by reusing sigmatch.ParamTypesMatch.
    # It's good enough for now.
    result = newNodeI(a.kind, n.info)
    for i in countup(0, len(a)-1): 
      var candidate = a.sons[i].sym
      if candidate.kind in {skProc, skMethod, skConverter, skIterator}: 
        # if suffices that the candidate has the proper number of generic 
        # type parameters:
        if safeLen(candidate.ast.sons[genericParamsPos]) == n.len-1:
          result.add(explicitGenericSym(c, n, candidate))
    # get rid of nkClosedSymChoice if not ambiguous:
    if result.len == 1 and a.kind == nkClosedSymChoice:
      result = result[0]
    # candidateCount != 1: return explicitGenericInstError(n)
  else:
    result = explicitGenericInstError(n)

proc SearchForBorrowProc(c: PContext, startScope: PScope, fn: PSym): PSym =
  # Searchs for the fn in the symbol table. If the parameter lists are suitable
  # for borrowing the sym in the symbol table is returned, else nil.
  # New approach: generate fn(x, y, z) where x, y, z have the proper types
  # and use the overloading resolution mechanism:
  var call = newNode(nkCall)
  call.add(newIdentNode(fn.name, fn.info))
  for i in 1.. <fn.typ.n.len:
    let param = fn.typ.n.sons[i]
    let t = skipTypes(param.typ, abstractVar-{tyTypeDesc})
    call.add(newNodeIT(nkEmpty, fn.info, t.baseOfDistinct))
  var resolved = semOverloadedCall(c, call, call, {fn.kind})
  if resolved != nil:
    result = resolved.sons[0].sym
