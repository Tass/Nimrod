# ----------------- IO Part ------------------------------------------------
type
  CFile {.importc: "FILE", nodecl, final, incompletestruct.} = object
  TFile* = ptr CFile ## The type representing a file handle.

  TFileMode* = enum           ## The file mode when opening a file.
    fmRead,                   ## Open the file for read access only.
    fmWrite,                  ## Open the file for write access only.
    fmReadWrite,              ## Open the file for read and write access.
                              ## If the file does not exist, it will be
                              ## created.
    fmReadWriteExisting,      ## Open the file for read and write access.
                              ## If the file does not exist, it will not be
                              ## created.
    fmAppend                  ## Open the file for writing only; append data
                              ## at the end.

  TFileHandle* = cint ## type that represents an OS file handle; this is
                      ## useful for low-level file access

# text file handling:
var
  stdin* {.importc: "stdin", noDecl.}: TFile   ## The standard input stream.
  stdout* {.importc: "stdout", noDecl.}: TFile ## The standard output stream.
  stderr* {.importc: "stderr", noDecl.}: TFile
    ## The standard error stream.
    ##
    ## Note: In my opinion, this should not be used -- the concept of a
    ## separate error stream is a design flaw of UNIX. A separate *message
    ## stream* is a good idea, but since it is named ``stderr`` there are few
    ## programs out there that distinguish properly between ``stdout`` and
    ## ``stderr``. So, that's what you get if you don't name your variables
    ## appropriately. It also annoys people if redirection
    ## via ``>output.txt`` does not work because the program writes
    ## to ``stderr``.

proc Open*(f: var TFile, filename: string,
         mode: TFileMode = fmRead, bufSize: int = -1): Bool {.tags: [].}
## Opens a file named `filename` with given `mode`.
##
## Default mode is readonly. Returns true iff the file could be opened.
## This throws no exception if the file could not be opened.

proc Open*(f: var TFile, filehandle: TFileHandle,
         mode: TFileMode = fmRead): Bool {.tags: [].}
## Creates a ``TFile`` from a `filehandle` with given `mode`.
##
## Default mode is readonly. Returns true iff the file could be opened.

proc Open*(filename: string,
         mode: TFileMode = fmRead, bufSize: int = -1): TFile = 
  ## Opens a file named `filename` with given `mode`.
  ##
  ## Default mode is readonly. Raises an ``IO`` exception if the file
  ## could not be opened.
  if not open(result, filename, mode, bufSize):
    raise newException(EIO, "cannot open: " & filename)

proc reopen*(f: TFile, filename: string, mode: TFileMode = fmRead): bool {. tags: [].}
## reopens the file `f` with given `filename` and `mode`. This 
## is often used to redirect the `stdin`, `stdout` or `stderr`
## file variables.
##
## Default mode is readonly. Returns true iff the file could be reopened.

proc Close*(f: TFile) {.importc: "fclose", nodecl, tags: [].}
## Closes the file.

proc EndOfFile*(f: TFile): Bool {.tags: [].}
## Returns true iff `f` is at the end.

proc readChar*(f: TFile): char {.importc: "fgetc", nodecl, tags: [FReadIO].}
## Reads a single character from the stream `f`.
proc FlushFile*(f: TFile) {.importc: "fflush", noDecl, tags: [FWriteIO].}
## Flushes `f`'s buffer.

proc readAll*(file: TFile): TaintedString {.tags: [FReadIO].}
## Reads all data from the stream `file`. Raises an IO exception
## in case of an error

proc readFile*(filename: string): TaintedString {.tags: [FReadIO].}
## Opens a file named `filename` for reading. Then calls `readAll`
## and closes the file afterwards. Returns the string. 
## Raises an IO exception in case of an error.

proc writeFile*(filename, content: string) {.tags: [FWriteIO].}
## Opens a file named `filename` for writing. Then writes the
## `content` completely to the file and closes the file afterwards.
## Raises an IO exception in case of an error.

proc write*(f: TFile, r: float) {.tags: [FWriteIO].}
proc write*(f: TFile, i: int) {.tags: [FWriteIO].}
proc write*(f: TFile, i: biggestInt) {.tags: [FWriteIO].}
proc write*(f: TFile, r: biggestFloat) {.tags: [FWriteIO].}
proc write*(f: TFile, s: string) {.tags: [FWriteIO].}
proc write*(f: TFile, b: Bool) {.tags: [FWriteIO].}
proc write*(f: TFile, c: char) {.tags: [FWriteIO].}
proc write*(f: TFile, c: cstring) {.tags: [FWriteIO].}
proc write*(f: TFile, a: varargs[string, `$`]) {.tags: [FWriteIO].}
## Writes a value to the file `f`. May throw an IO exception.

proc readLine*(f: TFile): TaintedString  {.tags: [FReadIO].}
## reads a line of text from the file `f`. May throw an IO exception.
## A line of text may be delimited by ``CR``, ``LF`` or
## ``CRLF``. The newline character(s) are not part of the returned string.

proc readLine*(f: TFile, line: var TaintedString): bool {.tags: [FReadIO].}
## reads a line of text from the file `f` into `line`. `line` must not be
## ``nil``! May throw an IO exception.
## A line of text may be delimited by ``CR``, ``LF`` or
## ``CRLF``. The newline character(s) are not part of the returned string.
## Returns ``false`` if the end of the file has been reached, ``true``
## otherwise. If ``false`` is returned `line` contains no new data.

proc writeln*[Ty](f: TFile, x: varargs[Ty, `$`]) {.inline, tags: [FWriteIO].}
## writes the values `x` to `f` and then writes "\n".
## May throw an IO exception.

proc getFileSize*(f: TFile): int64 {.tags: [FReadIO].}
## retrieves the file size (in bytes) of `f`.

proc ReadBytes*(f: TFile, a: var openarray[int8], start, len: int): int {.tags: [FReadIO].}
## reads `len` bytes into the buffer `a` starting at ``a[start]``. Returns
## the actual number of bytes that have been read which may be less than
## `len` (if not as many bytes are remaining), but not greater.

proc ReadChars*(f: TFile, a: var openarray[char], start, len: int): int {.tags: [FReadIO].}
## reads `len` bytes into the buffer `a` starting at ``a[start]``. Returns
## the actual number of bytes that have been read which may be less than
## `len` (if not as many bytes are remaining), but not greater.

proc readBuffer*(f: TFile, buffer: pointer, len: int): int {.tags: [FReadIO].}
## reads `len` bytes into the buffer pointed to by `buffer`. Returns
## the actual number of bytes that have been read which may be less than
## `len` (if not as many bytes are remaining), but not greater.

proc writeBytes*(f: TFile, a: openarray[int8], start, len: int): int {.tags: [FWriteIO].}
## writes the bytes of ``a[start..start+len-1]`` to the file `f`. Returns
## the number of actual written bytes, which may be less than `len` in case
## of an error.

proc writeChars*(f: tFile, a: openarray[char], start, len: int): int {.tags: [FWriteIO].}
## writes the bytes of ``a[start..start+len-1]`` to the file `f`. Returns
## the number of actual written bytes, which may be less than `len` in case
## of an error.

proc writeBuffer*(f: TFile, buffer: pointer, len: int): int {.tags: [FWriteIO].}
## writes the bytes of buffer pointed to by the parameter `buffer` to the
## file `f`. Returns the number of actual written bytes, which may be less
## than `len` in case of an error.

proc setFilePos*(f: TFile, pos: int64)
## sets the position of the file pointer that is used for read/write
## operations. The file's first byte has the index zero.

proc getFilePos*(f: TFile): int64
## retrieves the current position of the file pointer that is used to
## read from the file `f`. The file's first byte has the index zero.

proc fileHandle*(f: TFile): TFileHandle {.importc: "fileno",
                                        header: "<stdio.h>"}
## returns the OS file handle of the file ``f``. This is only useful for
## platform specific programming.

{.push stack_trace: off, profiler:off.}
when not defined(NimrodVM) and defined(windows):
  # work-around C's sucking abstraction:
  # BUGFIX: stdin and stdout should be binary files!
  proc setmode(handle, mode: int) {.importc: pccHack & "setmode",
                                    header: "<io.h>".}
  proc fileno(f: C_TextFileStar): int {.importc: pccHack & "fileno",
                                        header: "<fcntl.h>".}
  var
    O_BINARY {.importc: pccHack & "O_BINARY", nodecl.}: int

  # we use binary mode in Windows:
  setmode(fileno(c_stdin), O_BINARY)
  setmode(fileno(c_stdout), O_BINARY)
{.pop.}
include "system/sysio"

iterator lines*(filename: string): TaintedString {.tags: [FReadIO].} =
  ## Iterate over any line in the file named `filename`.
  ## If the file does not exist `EIO` is raised.
  var f = open(filename)
  var res = TaintedString(newStringOfCap(80))
  while f.readLine(res): yield res
  close(f)

iterator lines*(f: TFile): TaintedString {.tags: [FReadIO].} =
  ## Iterate over any line in the file `f`.
  var res = TaintedString(newStringOfCap(80))
  while f.readLine(res): yield TaintedString(res)
