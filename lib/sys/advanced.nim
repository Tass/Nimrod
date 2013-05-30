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
  
proc len*[T](x: seq[T]): int {.magic: "LengthSeq", noSideEffect.}
  ## returns the length of an array, an openarray, a sequence or a string.
  ## This is rougly the same as ``high(T)-low(T)+1``, but its resulting type is
  ## always an int.
