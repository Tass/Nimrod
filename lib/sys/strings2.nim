# implementation note: These must all have the same magic value "ConStrStr" so
# that the merge optimization works properly. 

proc copy*(s: string, first = 0): string {.
  magic: "CopyStr", importc: "copyStr", noSideEffect, deprecated.}
proc copy*(s: string, first, last: int): string {.
  magic: "CopyStrLast", importc: "copyStrLast", noSideEffect, 
  deprecated.}
  ## copies a slice of `s` into a new string and returns this new
  ## string. The bounds `first` and `last` denote the indices of
  ## the first and last characters that shall be copied. If ``last``
  ## is omitted, it is treated as ``high(s)``.
  ## **Deprecated since version 0.8.12**: Use ``substr`` instead.

proc substr*(s: string, first = 0): string {.
  magic: "CopyStr", importc: "copyStr", noSideEffect.}
proc substr*(s: string, first, last: int): string {.
  magic: "CopyStrLast", importc: "copyStrLast", noSideEffect.}
  ## copies a slice of `s` into a new string and returns this new
  ## string. The bounds `first` and `last` denote the indices of
  ## the first and last characters that shall be copied. If ``last``
  ## is omitted, it is treated as ``high(s)``. If ``last >= s.len``, ``s.len``
  ## is used instead: This means ``substr`` can also be used to `cut`:idx:
  ## or `limit`:idx: a string's length.

iterator items*[T](a: seq[T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while i < len(a):
    yield a[i]
    inc(i)

iterator items*(a: string): char {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while i < len(a):
    yield a[i]
    inc(i)

iterator pairs*[T](a: seq[T]): tuple[key: int, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)

iterator pairs*(a: string): tuple[key: int, val: char] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)

proc isNil*[T](x: seq[T]): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: string): bool {.noSideEffect, magic: "IsNil".}

proc `@`*[T](a: openArray[T]): seq[T] = 
  ## turns an openarray into a sequence. This is not as efficient as turning
  ## a fixed length array into a sequence as it always copies every element
  ## of `a`.
  newSeq(result, a.len)
  for i in 0..a.len-1: result[i] = a[i]

proc `&` *[T](x, y: seq[T]): seq[T] {.noSideEffect.} =
  newSeq(result, x.len + y.len)
  for i in 0..x.len-1:
    result[i] = x[i]
  for i in 0..y.len-1:
    result[i+x.len] = y[i]

proc `&` *[T](x: seq[T], y: T): seq[T] {.noSideEffect.} =
  newSeq(result, x.len + 1)
  for i in 0..x.len-1:
    result[i] = x[i]
  result[x.len] = y

proc `&` *[T](x: T, y: seq[T]): seq[T] {.noSideEffect.} =
  newSeq(result, y.len + 1)
  for i in 0..y.len-1:
    result[i] = y[i]
  result[y.len] = x

when not defined(NimrodVM):
  when not defined(JS):
    proc seqToPtr[T](x: seq[T]): pointer {.inline, nosideeffect.} =
      result = cast[pointer](x)
  else:
    proc seqToPtr[T](x: seq[T]): pointer {.noStackFrame, nosideeffect.} =
      asm """return `x`"""
  
  proc `==` *[T: typeDesc](x, y: seq[T]): bool {.noSideEffect.} =
    ## Generic equals operator for sequences: relies on a equals operator for
    ## the element type `T`.
    if seqToPtr(x) == seqToPtr(y):
      result = true
    elif seqToPtr(x) == nil or seqToPtr(y) == nil:
      result = false
    elif x.len == y.len:
      for i in 0..x.len-1:
        if x[i] != y[i]: return false
      result = true

proc each*[T, S](data: openArray[T], op: proc (x: T): S {.closure.}): seq[S] {.
  deprecated.} =
  ## The well-known ``map`` operation from functional programming. Applies
  ## `op` to every item in `data` and returns the result as a sequence.
  ##
  ## **Deprecated since version 0.9:** Use the ``map`` proc instead.
  newSeq(result, data.len)
  for i in 0..data.len-1: result[i] = op(data[i])

proc each*[T](data: var openArray[T], op: proc (x: var T) {.closure.}) {.
  deprecated.} =
  ## The well-known ``map`` operation from functional programming. Applies
  ## `op` to every item in `data` modifying it directly.
  ##
  ## **Deprecated since version 0.9:** Use the ``map`` proc instead.
  for i in 0..data.len-1: op(data[i])

proc map*[T, S](data: openArray[T], op: proc (x: T): S {.closure.}): seq[S] =
  ## Returns a new sequence with the results of `op` applied to every item in
  ## `data`.
  ##
  ## Since the input is not modified you can use this version of ``map`` to
  ## transform the type of the elements in the input sequence. Example:
  ##
  ## .. code-block:: nimrod
  ##   let
  ##     a = @[1, 2, 3, 4]
  ##     b = map(a, proc(x: int): string = $x)
  ##   assert b == @["1", "2", "3", "4"]
  newSeq(result, data.len)
  for i in 0..data.len-1: result[i] = op(data[i])

proc map*[T](data: var openArray[T], op: proc (x: var T) {.closure.}) =
  ## Applies `op` to every item in `data` modifying it directly.
  ##
  ## Note that this version of ``map`` requires your input and output types to
  ## be the same, since they are modified in-place. Example:
  ##
  ## .. code-block:: nimrod
  ##   var a = @["1", "2", "3", "4"]
  ##   echo repr(a)
  ##   # --> ["1", "2", "3", "4"]
  ##   map(a, proc(x: var string) = x &= "42")
  ##   echo repr(a)
  ##   # --> ["142", "242", "342", "442"]
  for i in 0..data.len-1: op(data[i])

proc `$`*[T: set](x: T): string = 
  ## generic ``$`` operator for sets that is lifted from the components
  ## of `x`. Example:
  ##
  ## .. code-block:: nimrod
  ##   ${23, 45} == "{23, 45}"
  result = "{"
  for value in items(x):
    if result.len > 1: result.add(", ")
    result.add($value)
  result.add("}")

when false:
  proc `$`*[T](a: openArray[T]): string = 
    ## generic ``$`` operator for open arrays that is lifted from the elements
    ## of `a`. Example:
    ##
    ## .. code-block:: nimrod
    ##   $[23, 45] == "[23, 45]"
    result = "["
    for x in items(a):
      if result.len > 1: result.add(", ")
      result.add($x)
    result.add("]")

template accumulateResult*(iter: expr) =
  ## helps to convert an iterator to a proc.
  result = @[]
  for x in iter: add(result, x)

proc debugEcho*[T](x: varargs[T, `$`]) {.magic: "Echo", noSideEffect, 
                                         tags: [], raises: [].}
  ## Same as ``echo``, but as a special semantic rule, ``debugEcho`` pretends
  ## to be free of side effects, so that it can be used for debugging routines
  ## marked as ``noSideEffect``.

template `-|`*(b, s: expr): expr =
  (if b >= 0: b else: s.len + b)

proc `[]`*(s: string, x: TSlice[int]): string {.inline.} =
  ## slice operation for strings. Negative indexes are supported.
  result = s.substr(x.a-|s, x.b-|s)

proc `[]`*[Idx, T](a: array[Idx, T], x: TSlice[int]): seq[T] =
  ## slice operation for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = x.b - x.a + 1
  newSeq(result, L)
  for i in 0.. <L: result[i] = a[i + x.a]

proc `[]`*[Idx, T](a: array[Idx, T], x: TSlice[Idx]): seq[T] =
  ## slice operation for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = ord(x.b) - ord(x.a) + 1
  newSeq(result, L)
  var j = x.a
  for i in 0.. <L: 
    result[i] = a[j]
    inc(j)

proc `[]`*[T](s: seq[T], x: TSlice[int]): seq[T] = 
  ## slice operation for sequences. Negative indexes are supported.
  var a = x.a-|s
  var L = x.b-|s - a + 1
  newSeq(result, L)
  for i in 0.. <L: result[i] = s[i + a]

proc insert*(x: var string, item: string, i = 0) {.noSideEffect.} = 
  ## inserts `item` into `x` at position `i`.
  var xl = x.len
  setLen(x, xl+item.len)
  var j = xl-1
  while j >= i:
    shallowCopy(x[j+item.len], x[j])
    dec(j)
  j = 0
  while j < item.len:
    x[j+i] = item[j]
    inc(j)

# XXX: make these the default (or implement the NilObject optimization)
proc safeAdd*[T](x: var seq[T], y: T) {.noSideEffect.} =
  if x == nil: x = @[y]
  else: x.add(y)

proc safeAdd*(x: var string, y: char) =
  if x == nil: x = ""
  x.add(y)

proc safeAdd*(x: var string, y: string) =
  if x == nil: x = y
  else: x.add(y)

proc pop*[T](s: var seq[T]): T {.inline, noSideEffect.} = 
  ## returns the last item of `s` and decreases ``s.len`` by one. This treats
  ## `s` as a stack and implements the common *pop* operation.
  var L = s.len-1
  result = s[L]
  setLen(s, L)

proc shallow*[T](s: var seq[T]) {.noSideEffect, inline.} =
  ## marks a sequence `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`. This is only useful for optimization 
  ## purposes.
  when not defined(JS) and not defined(NimrodVM):
    var s = cast[PGenericSeq](s)
    s.reserved = s.reserved or seqShallowFlag
proc `$`*[T: tuple|object](x: T): string = 
  ## generic ``$`` operator for tuples that is lifted from the components
  ## of `x`. Example:
  ##
  ## .. code-block:: nimrod
  ##   $(23, 45) == "(23, 45)"
  ##   $() == "()"
  result = "("
  for name, value in fieldPairs(x):
    if result.len > 1: result.add(", ")
    result.add(name)
    result.add(": ")
    result.add($value)
  result.add(")")

template spliceImpl(s, a, L, b: expr): stmt {.immediate.} =
  # make room for additional elements or cut:
  var slen = s.len
  var shift = b.len - L
  var newLen = slen + shift
  if shift > 0:
    # enlarge:
    setLen(s, newLen)
    for i in countdown(newLen-1, a+shift+1): shallowCopy(s[i], s[i-shift])
  else:
    for i in countup(a+b.len, s.len-1+shift): shallowCopy(s[i], s[i-shift])
    # cut down:
    setLen(s, newLen)
  # fill the hole:
  for i in 0 .. <b.len: s[i+a] = b[i]  

proc `[]=`*[T](s: var seq[T], x: TSlice[int], b: openArray[T]) = 
  ## slice assignment for sequences. Negative indexes are supported. If
  ## ``b.len`` is not exactly the number of elements that are referred to
  ## by `x`, a `splice`:idx: is performed. 
  var a = x.a-|s
  var L = x.b-|s - a + 1
  if L == b.len:
    for i in 0 .. <L: s[i+a] = b[i]
  else:
    spliceImpl(s, a, L, b)
  
proc `[]=`*(s: var string, x: TSlice[int], b: string) = 
  ## slice assignment for strings. Negative indexes are supported. If
  ## ``b.len`` is not exactly the number of elements that are referred to
  ## by `x`, a `splice`:idx: is performed:
  ##
  ## .. code-block:: nimrod
  ##   var s = "abcdef"
  ##   s[1 .. -2] = "xyz"
  ##   assert s == "axyzf"
  var a = x.a-|s
  var L = x.b-|s - a + 1
  if L == b.len:
    for i in 0 .. <L: s[i+a] = b[i]
  else:
    spliceImpl(s, a, L, b)

proc `[]=`*[Idx, T](a: var array[Idx, T], x: TSlice[int], b: openArray[T]) =
  ## slice assignment for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = x.b - x.a + 1
  if L == b.len:
    for i in 0 .. <L: a[i+x.a] = b[i]
  else:
    raise newException(EOutOfRange, "differing lengths for slice assignment")

proc `[]=`*[Idx, T](a: var array[Idx, T], x: TSlice[Idx], b: openArray[T]) =
  ## slice assignment for arrays. Negative indexes are **not** supported
  ## because the array might have negative bounds.
  var L = ord(x.b) - ord(x.a) + 1
  if L == b.len:
    var j = x.a
    for i in 0 .. <L: 
      a[j] = b[i]
      inc(j)
  else:
    raise newException(EOutOfRange, "differing lengths for slice assignment")
