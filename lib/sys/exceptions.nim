# This file requires strings, at least for now.

# we have to compute this here before turning it off in except.nim anyway ...
const nimrodStackTrace = compileOption("stacktrace")

{.push checks: off.}
# obviously we cannot generate checking operations here :-)
# because it would yield into an endless recursion
# however, stack-traces are available for most parts
# of the code

var
  globalRaiseHook*: proc (e: ref E_Base): bool {.nimcall.}
    ## with this hook you can influence exception handling on a global level.
    ## If not nil, every 'raise' statement ends up calling this hook. Ordinary
    ## application code should never set this hook! You better know what you
    ## do when setting this. If ``globalRaiseHook`` returns false, the
    ## exception is caught and does not propagate further through the call
    ## stack.

  localRaiseHook* {.threadvar.}: proc (e: ref E_Base): bool {.nimcall.}
    ## with this hook you can influence exception handling on a
    ## thread local level.
    ## If not nil, every 'raise' statement ends up calling this hook. Ordinary
    ## application code should never set this hook! You better know what you
    ## do when setting this. If ``localRaiseHook`` returns false, the exception
    ## is caught and does not propagate further through the call stack.
    
  outOfMemHook*: proc () {.nimcall, tags: [].}
    ## set this variable to provide a procedure that should be called 
    ## in case of an `out of memory`:idx: event. The standard handler
    ## writes an error message and terminates the program. `outOfMemHook` can
    ## be used to raise an exception in case of OOM like so:
    ## 
    ## .. code-block:: nimrod
    ##
    ##   var gOutOfMem: ref EOutOfMemory
    ##   new(gOutOfMem) # need to be allocated *before* OOM really happened!
    ##   gOutOfMem.msg = "out of memory"
    ## 
    ##   proc handleOOM() =
    ##     raise gOutOfMem
    ##
    ##   system.outOfMemHook = handleOOM
    ##
    ## If the handler does not raise an exception, ordinary control flow
    ## continues and the program is terminated.

template newException*(exceptn: typeDesc, message: string): expr =
  ## creates an exception object of type ``exceptn`` and sets its ``msg`` field
  ## to `message`. Returns the new exception object.
  var
    e: ref exceptn
  new(e)
  e.msg = message
  e
