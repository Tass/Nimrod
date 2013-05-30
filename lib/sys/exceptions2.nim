when not defined(JS):
  proc getCurrentException*(): ref E_Base {.compilerRtl, inl.} =
    ## retrieves the current exception; if there is none, nil is returned.
    result = currException

  proc getCurrentExceptionMsg*(): string {.inline.} =
    ## retrieves the error message that was attached to the current
    ## exception; if there is none, "" is returned.
    var e = getCurrentException()
    return if e == nil: "" else: e.msg

  proc onRaise*(action: proc(e: ref E_Base): bool{.closure.}) =
    ## can be used in a ``try`` statement to setup a Lisp-like
    ## `condition system`:idx:\: This prevents the 'raise' statement to
    ## raise an exception but instead calls ``action``.
    ## If ``action`` returns false, the exception has been handled and
    ## does not propagate further through the call stack.
    if not isNil(excHandler):
      excHandler.hasRaiseAction = true
      excHandler.raiseAction = action
