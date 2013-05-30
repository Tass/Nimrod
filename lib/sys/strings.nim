{.push hints: off.}

type
  seq*{.magic: "Seq".}[T]  ## Generic type to construct sequences.

when not defined(JS) and not defined(NimrodVM):
  type
    TGenericSeq {.compilerproc, pure, inheritable.} = object
      len, reserved: int
    PGenericSeq {.exportc.} = ptr TGenericSeq
    # len and space without counting the terminating zero:
    NimStringDesc {.compilerproc, final.} = object of TGenericSeq
      data: array[0..100_000_000, char]
    NimString = ptr NimStringDesc
    
  template space(s: PGenericSeq): int {.dirty.} =
    s.reserved and not seqShallowFlag

proc newSeq*[T](s: var seq[T], len: int) {.magic: "NewSeq", noSideEffect.}
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ## This is equivalent to ``s = @[]; setlen(s, len)``, but more
  ## efficient since no reallocation is needed.
  ##
  ## Note that the sequence will be filled with uninitialized entries, which
  ## can be a problem for sequences containing strings. After the creation of
  ## the sequence you should assign entries to the sequence instead of adding
  ## them. Example:
  ##
  ## .. code-block:: nimrod
  ##   var inputStrings : seq[string]
  ##   newSeq(inputStrings, 3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"

proc newSeq*[T](len = 0): seq[T] =
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## Note that the sequence will be filled with uninitialized entries, which
  ## can be a problem for sequences containing strings. After the creation of
  ## the sequence you should assign entries to the sequence instead of adding
  ## them. Example:
  ##
  ## .. code-block:: nimrod
  ##   var inputStrings = newSeq[string](3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"
  newSeq(result, len)

proc len*(x: string): int {.magic: "LengthStr", noSideEffect.}
proc len*[T](x: seq[T]): int {.magic: "LengthSeq", noSideEffect.}

proc `@` * [IDX, T](a: array[IDX, T]): seq[T] {.
  magic: "ArrToSeq", nosideeffect.}
  ## turns an array into a sequence. This most often useful for constructing
  ## sequences with the array constructor: ``@[1, 2, 3]`` has the type 
  ## ``seq[int]``, while ``[1, 2, 3]`` has the type ``array[0..2, int]``.

proc setLen*[T](s: var seq[T], newlen: int) {.
  magic: "SetLengthSeq", noSideEffect.}
  ## sets the length of `s` to `newlen`.
  ## ``T`` may be any sequence type.
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated. `s` cannot be nil! To initialize a sequence with
  ## a size, use ``newSeq`` instead. 

proc setLen*(s: var string, newlen: int) {.
  magic: "SetLengthStr", noSideEffect.}
  ## sets the length of `s` to `newlen`.
  ## If the current length is greater than the new length,
  ## ``s`` will be truncated. `s` cannot be nil! To initialize a string with
  ## a size, use ``newString`` instead. 

proc newString*(len: int): string {.
  magic: "NewString", importc: "mnewString", noSideEffect.}
  ## returns a new string of length ``len`` but with uninitialized
  ## content. One needs to fill the string character after character
  ## with the index operator ``s[i]``. This procedure exists only for
  ## optimization purposes; the same effect can be achieved with the
  ## ``&`` operator or with ``add``.

proc newStringOfCap*(cap: int): string {.
  magic: "NewStringOfCap", importc: "rawNewString", noSideEffect.}
  ## returns a new string of length ``0`` but with capacity `cap`.This
  ## procedure exists only for optimization purposes; the same effect can 
  ## be achieved with the ``&`` operator or with ``add``.

proc `&` * (x: string, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
proc `&` * (x: char, y: char): string {.
  magic: "ConStrStr", noSideEffect, merge.}
proc `&` * (x, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
proc `&` * (x: char, y: string): string {.
  magic: "ConStrStr", noSideEffect, merge.}
  ## is the `concatenation operator`. It concatenates `x` and `y`.

const
  taintMode = compileOption("taintmode")

when taintMode:
  type TaintedString* = distinct string ## a distinct string type that 
                                        ## is `tainted`:idx:. It is an alias for
                                        ## ``string`` if the taint mode is not
                                        ## turned on. Use the ``-d:taintMode``
                                        ## command line switch to turn the taint
                                        ## mode on.
  
  proc len*(s: TaintedString): int {.borrow.}
else:
  type TaintedString* = string          ## a distinct string type that 
                                        ## is `tainted`:idx:. It is an alias for
                                        ## ``string`` if the taint mode is not
                                        ## turned on. Use the ``-d:taintMode``
                                        ## command line switch to turn the taint
                                        ## mode on.

proc add*(x: var string, y: char) {.magic: "AppendStrCh", noSideEffect.}
proc add*(x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}

proc add *[T](x: var seq[T], y: T) {.magic: "AppendSeqElem", noSideEffect.}
proc add *[T](x: var seq[T], y: openArray[T]) {.noSideEffect.} =
  ## Generic proc for adding a data item `y` to a container `x`.
  ## For containers that have an order, `add` means *append*. New generic
  ## containers should also call their adding proc `add` for consistency.
  ## Generic code becomes much easier to write if the Nimrod naming scheme is
  ## respected.
  var xl = x.len
  setLen(x, xl + y.len)
  for i in 0..high(y): x[xl+i] = y[i]

proc shallowCopy*[T](x: var T, y: T) {.noSideEffect, magic: "ShallowCopy".}
  ## use this instead of `=` for a `shallow copy`:idx:. The shallow copy
  ## only changes the semantics for sequences and strings (and types which
  ## contain those). Be careful with the changed semantics though! There 
  ## is a reason why the default assignment does a deep copy of sequences
  ## and strings.

proc del*[T](x: var seq[T], i: int) {.noSideEffect.} = 
  ## deletes the item at index `i` by putting ``x[high(x)]`` into position `i`.
  ## This is an O(1) operation.
  var xl = x.len
  shallowCopy(x[i], x[xl-1])
  setLen(x, xl-1)
  
proc delete*[T](x: var seq[T], i: int) {.noSideEffect.} = 
  ## deletes the item at index `i` by moving ``x[i+1..]`` by one position.
  ## This is an O(n) operation.
  var xl = x.len
  for j in i..xl-2: shallowCopy(x[j], x[j+1]) 
  setLen(x, xl-1)
  
proc insert*[T](x: var seq[T], item: T, i = 0) {.noSideEffect.} = 
  ## inserts `item` into `x` at position `i`.
  var xl = x.len
  setLen(x, xl+1)
  var j = xl-1
  while j >= i:
    shallowCopy(x[j+1], x[j])
    dec(j)
  x[i] = item

proc repr*[T](x: T): string {.magic: "Repr", noSideEffect.}
  ## takes any Nimrod variable and returns its string representation. It
  ## works even for complex data graphs with cycles. This is a great
  ## debugging tool.

proc echo*[T](x: varargs[T, `$`]) {.magic: "Echo", tags: [FWriteIO].}
  ## special built-in that takes a variable number of arguments. Each argument
  ## is converted to a string via ``$``, so it works for user-defined
  ## types that have an overloaded ``$`` operator.
  ## It is roughly equivalent to ``writeln(stdout, x); flush(stdout)``, but
  ## available for the JavaScript target too.
  ## Unlike other IO operations this is guaranteed to be thread-safe as
  ## ``echo`` is very often used for debugging convenience.

proc `$` *(x: int): string {.magic: "IntToStr", noSideEffect.}
  ## The stingify operator for an integer argument. Returns `x`
  ## converted to a decimal string.

proc `$` *(x: int64): string {.magic: "Int64ToStr", noSideEffect.}
  ## The stingify operator for an integer argument. Returns `x`
  ## converted to a decimal string.
when not defined(NimrodVM):
  when not defined(JS):
    proc `$` *(x: uint64): string {.noSideEffect.}
      ## The stingify operator for an unsigned integer argument. Returns `x`
      ## converted to a decimal string.

proc `$` *(x: float): string {.magic: "FloatToStr", noSideEffect.}
  ## The stingify operator for a float argument. Returns `x`
  ## converted to a decimal string.

proc `$` *(x: bool): string {.magic: "BoolToStr", noSideEffect.}
  ## The stingify operator for a boolean argument. Returns `x`
  ## converted to the string "false" or "true".

proc `$` *(x: char): string {.magic: "CharToStr", noSideEffect.}
  ## The stingify operator for a character argument. Returns `x`
  ## converted to a string.

proc `$` *(x: Cstring): string {.magic: "CStrToStr", noSideEffect.}
  ## The stingify operator for a CString argument. Returns `x`
  ## converted to a string.

proc `$` *(x: string): string {.magic: "StrToStr", noSideEffect.}
  ## The stingify operator for a string argument. Returns `x`
  ## as it is. This operator is useful for generic code, so
  ## that ``$expr`` also works if ``expr`` is already a string.

proc `$` *[TEnum: enum](x: TEnum): string {.magic: "EnumToStr", noSideEffect.}
  ## The stingify operator for an enumeration argument. This works for
  ## any enumeration type thanks to compiler magic. If
  ## a ``$`` operator for a concrete enumeration is provided, this is
  ## used instead. (In other words: *Overwriting* is possible.)
