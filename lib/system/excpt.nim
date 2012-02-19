#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Exception handling code. Carefully coded so that tiny programs which do not
# use the heap (and nor exceptions) do not include the GC or memory allocator.

var
  stackTraceNewLine*: string ## undocumented feature; it is replaced by ``<br>``
                             ## for CGI applications

template stackTraceNL: expr =
  (if IsNil(stackTraceNewLine): "\n" else: stackTraceNewLine)

when not defined(windows) or not defined(guiapp):
  proc writeToStdErr(msg: CString) = write(stdout, msg)

else:
  proc MessageBoxA(hWnd: cint, lpText, lpCaption: cstring, uType: int): int32 {.
    header: "<windows.h>", nodecl.}

  proc writeToStdErr(msg: CString) =
    discard MessageBoxA(0, msg, nil, 0)

proc registerSignalHandler() {.compilerproc.}

proc chckIndx(i, a, b: int): int {.inline, compilerproc.}
proc chckRange(i, a, b: int): int {.inline, compilerproc.}
proc chckRangeF(x, a, b: float): float {.inline, compilerproc.}
proc chckNil(p: pointer) {.inline, compilerproc.}

var
  framePtr {.rtlThreadVar.}: PFrame
  excHandler {.rtlThreadVar.}: PSafePoint
    # list of exception handlers
    # a global variable for the root of all try blocks
  currException {.rtlThreadVar.}: ref E_Base

proc pushFrame(s: PFrame) {.compilerRtl, inl.} = 
  s.prev = framePtr
  framePtr = s

proc popFrame {.compilerRtl, inl.} =
  framePtr = framePtr.prev

proc setFrame(s: PFrame) {.compilerRtl, inl.} =
  framePtr = s

proc pushSafePoint(s: PSafePoint) {.compilerRtl, inl.} = 
  s.prev = excHandler
  excHandler = s

proc popSafePoint {.compilerRtl, inl.} =
  excHandler = excHandler.prev

proc pushCurrentException(e: ref E_Base) {.compilerRtl, inl.} = 
  e.parent = currException
  currException = e

proc popCurrentException {.compilerRtl, inl.} =
  currException = currException.parent

# some platforms have native support for stack traces:
const
  nativeStackTraceSupported = (defined(macosx) or defined(linux)) and 
                              not nimrodStackTrace
  hasSomeStackTrace = nimrodStackTrace or 
    defined(nativeStackTrace) and nativeStackTraceSupported

when defined(nativeStacktrace) and nativeStackTraceSupported:
  type
    TDl_info {.importc: "Dl_info", header: "<dlfcn.h>", 
               final, pure.} = object
      dli_fname: CString
      dli_fbase: pointer
      dli_sname: CString
      dli_saddr: pointer

  proc backtrace(symbols: ptr pointer, size: int): int {.
    importc: "backtrace", header: "<execinfo.h>".}
  proc dladdr(addr1: pointer, info: ptr TDl_info): int {.
    importc: "dladdr", header: "<dlfcn.h>".}

  when not hasThreadSupport:
    var
      tempAddresses: array [0..127, pointer] # should not be alloc'd on stack
      tempDlInfo: TDl_info

  proc auxWriteStackTraceWithBacktrace(s: var string) =
    when hasThreadSupport:
      var
        tempAddresses: array [0..127, pointer] # but better than a threadvar
        tempDlInfo: TDl_info
    # This is allowed to be expensive since it only happens during crashes
    # (but this way you don't need manual stack tracing)
    var size = backtrace(cast[ptr pointer](addr(tempAddresses)), 
                         len(tempAddresses))
    var enabled = false
    for i in 0..size-1:
      var dlresult = dladdr(tempAddresses[i], addr(tempDlInfo))
      if enabled:
        if dlresult != 0:
          var oldLen = s.len
          add(s, tempDlInfo.dli_fname)
          if tempDlInfo.dli_sname != nil:
            for k in 1..max(1, 25-(s.len-oldLen)): add(s, ' ')
            add(s, tempDlInfo.dli_sname)
        else:
          add(s, '?')
        add(s, stackTraceNL)
      else:
        if dlresult != 0 and tempDlInfo.dli_sname != nil and
            c_strcmp(tempDlInfo.dli_sname, "signalHandler") == 0'i32:
          # Once we're past signalHandler, we're at what the user is
          # interested in
          enabled = true

when not hasThreadSupport:
  var
    tempFrames: array [0..127, PFrame] # should not be alloc'd on stack
  
proc auxWriteStackTrace(f: PFrame, s: var string) =
  when hasThreadSupport:
    var
      tempFrames: array [0..127, PFrame] # but better than a threadvar
  const
    firstCalls = 32
  var
    it = f
    i = 0
    total = 0
  while it != nil and i <= high(tempFrames)-(firstCalls-1):
    # the (-1) is for a nil entry that marks where the '...' should occur
    tempFrames[i] = it
    inc(i)
    inc(total)
    it = it.prev
  var b = it
  while it != nil:
    inc(total)
    it = it.prev
  for j in 1..total-i-(firstCalls-1): 
    if b != nil: b = b.prev
  if total != i:
    tempFrames[i] = nil
    inc(i)
  while b != nil and i <= high(tempFrames):
    tempFrames[i] = b
    inc(i)
    b = b.prev
  for j in countdown(i-1, 0):
    if tempFrames[j] == nil: 
      add(s, "(")
      add(s, $(total-i-1))
      add(s, " calls omitted) ...")
    else:
      var oldLen = s.len
      add(s, tempFrames[j].filename)
      if tempFrames[j].line > 0:
        add(s, '(')
        add(s, $tempFrames[j].line)
        add(s, ')')
      for k in 1..max(1, 25-(s.len-oldLen)): add(s, ' ')
      add(s, tempFrames[j].procname)
    add(s, stackTraceNL)

when hasSomeStackTrace:
  proc rawWriteStackTrace(s: var string) =
    when nimrodStackTrace:
      if framePtr == nil:
        add(s, "No stack traceback available")
        add(s, stackTraceNL)
      else:
        add(s, "Traceback (most recent call last)")
        add(s, stackTraceNL)
        auxWriteStackTrace(framePtr, s)
    elif defined(nativeStackTrace) and nativeStackTraceSupported:
      add(s, "Traceback from system (most recent call last)")
      add(s, stackTraceNL)
      auxWriteStackTraceWithBacktrace(s)
    else:
      add(s, "No stack traceback available\n")

proc quitOrDebug() {.inline.} =
  when not defined(endb):
    quit(1)
  else:
    endbStep() # call the debugger

proc raiseException(e: ref E_Base, ename: CString) {.compilerRtl.} =
  e.name = ename
  if localRaiseHook != nil:
    if not localRaiseHook(e): return
  if globalRaiseHook != nil:
    if not globalRaiseHook(e): return
  if excHandler != nil:
    pushCurrentException(e)
    c_longjmp(excHandler.context, 1)
  elif e[] of EOutOfMemory:
    writeToStdErr(ename)
    quitOrDebug()
  else:
    when hasSomeStackTrace:
      var buf = newStringOfCap(2000)
      rawWriteStackTrace(buf)
      add(buf, "Error: unhandled exception: ")
      if not isNil(e.msg): add(buf, e.msg)
      add(buf, " [")
      add(buf, $ename)
      add(buf, "]\n")
      writeToStdErr(buf)
    else:
      # ugly, but avoids heap allocations :-)
      template xadd(buf, s, slen: expr) =
        if L + slen < high(buf):
          copyMem(addr(buf[L]), cstring(s), slen)
          inc L, slen
      template add(buf, s: expr) =
        xadd(buf, s, s.len)
      var buf: array [0..2000, char]
      var L = 0
      add(buf, "Error: unhandled exception: ")
      if not isNil(e.msg): add(buf, e.msg)
      add(buf, " [")
      xadd(buf, ename, c_strlen(ename))
      add(buf, "]\n")
      writeToStdErr(buf)
    quitOrDebug()

proc reraiseException() {.compilerRtl.} =
  if currException == nil:
    raise newException(ENoExceptionToReraise, "no exception to reraise")
  else:
    raiseException(currException, currException.name)

proc WriteStackTrace() =
  when hasSomeStackTrace:
    var s = ""
    rawWriteStackTrace(s)
    writeToStdErr(s)
  else:
    writeToStdErr("No stack traceback available\n")

when defined(endb):
  var
    dbgAborting: bool # whether the debugger wants to abort

proc signalHandler(sig: cint) {.exportc: "signalHandler", noconv.} =
  template processSignal(s, action: expr) =
    if s == SIGINT: action("SIGINT: Interrupted by Ctrl-C.\n")
    elif s == SIGSEGV: 
      action("SIGSEGV: Illegal storage access. (Attempt to read from nil?)\n")
    elif s == SIGABRT:
      when defined(endb):
        if dbgAborting: return # the debugger wants to abort
      action("SIGABRT: Abnormal termination.\n")
    elif s == SIGFPE: action("SIGFPE: Arithmetic error.\n")
    elif s == SIGILL: action("SIGILL: Illegal operation.\n")
    elif s == SIGBUS: 
      action("SIGBUS: Illegal storage access. (Attempt to read from nil?)\n")
    else: action("unknown signal\n")

  # print stack trace and quit
  when hasSomeStackTrace:
    GC_disable()
    var buf = newStringOfCap(2000)
    rawWriteStackTrace(buf)
    processSignal(sig, buf.add) # nice hu? currying a la nimrod :-)
    writeToStdErr(buf)
    GC_enable()
  else:
    var msg: cstring
    template asgn(y: expr) = msg = y
    processSignal(sig, asgn)
    writeToStdErr(msg)
  when defined(endb): dbgAborting = True
  quit(1) # always quit when SIGABRT

proc registerSignalHandler() =
  c_signal(SIGINT, signalHandler)
  c_signal(SIGSEGV, signalHandler)
  c_signal(SIGABRT, signalHandler)
  c_signal(SIGFPE, signalHandler)
  c_signal(SIGILL, signalHandler)
  c_signal(SIGBUS, signalHandler)

when not defined(noSignalHandler):
  registerSignalHandler() # call it in initialization section

proc setControlCHook(hook: proc () {.noconv.}) =
  # ugly cast, but should work on all architectures:
  type TSignalHandler = proc (sig: cint) {.noconv.}
  c_signal(SIGINT, cast[TSignalHandler](hook))

proc raiseRangeError(val: biggestInt) {.compilerproc, noreturn, noinline.} =
  raise newException(EOutOfRange, "value " & $val & " out of range")

proc raiseIndexError() {.compilerproc, noreturn, noinline.} =
  raise newException(EInvalidIndex, "index out of bounds")

proc raiseFieldError(f: string) {.compilerproc, noreturn, noinline.} =
  raise newException(EInvalidField, f & " is not accessible")

proc chckIndx(i, a, b: int): int =
  if i >= a and i <= b:
    return i
  else:
    raiseIndexError()

proc chckRange(i, a, b: int): int =
  if i >= a and i <= b:
    return i
  else:
    raiseRangeError(i)

proc chckRange64(i, a, b: int64): int64 {.compilerproc.} =
  if i >= a and i <= b:
    return i
  else:
    raiseRangeError(i)

proc chckRangeF(x, a, b: float): float =
  if x >= a and x <= b:
    return x
  else:
    raise newException(EOutOfRange, "value " & $x & " out of range")

proc chckNil(p: pointer) =
  if p == nil: c_raise(SIGSEGV)

proc chckObj(obj, subclass: PNimType) {.compilerproc.} =
  # checks if obj is of type subclass:
  var x = obj
  if x == subclass: return # optimized fast path
  while x != subclass:
    if x == nil:
      raise newException(EInvalidObjectConversion, "invalid object conversion")
    x = x.base

proc chckObjAsgn(a, b: PNimType) {.compilerproc, inline.} =
  if a != b:
    raise newException(EInvalidObjectAssignment, "invalid object assignment")

proc isObj(obj, subclass: PNimType): bool {.compilerproc.} =
  # checks if obj is of type subclass:
  var x = obj
  if x == subclass: return true # optimized fast path
  while x != subclass:
    if x == nil: return false
    x = x.base
  return true
