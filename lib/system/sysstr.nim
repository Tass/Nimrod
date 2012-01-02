#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# string & sequence handling procedures needed by the code generator

# strings are dynamically resized, have a length field
# and are zero-terminated, so they can be casted to C
# strings easily
# we don't use refcounts because that's a behaviour
# the programmer may not want

proc resize(old: int): int {.inline.} =
  if old <= 0: result = 4
  elif old < 65536: result = old * 2
  else: result = old * 3 div 2 # for large arrays * 3/2 is better

proc cmpStrings(a, b: NimString): int {.inline, compilerProc.} =
  if a == b: return 0
  if a == nil: return -1
  if b == nil: return 1
  return c_strcmp(a.data, b.data)

proc eqStrings(a, b: NimString): bool {.inline, compilerProc.} =
  if a == b: return true
  if a == nil or b == nil: return false
  return a.len == b.len and
    c_memcmp(a.data, b.data, a.len * sizeof(char)) == 0'i32

proc rawNewString(space: int): NimString {.compilerProc.} =
  var s = space
  if s < 8: s = 7
  result = cast[NimString](newObj(addr(strDesc), sizeof(TGenericSeq) +
                           (s+1) * sizeof(char)))
  result.space = s

proc mnewString(len: int): NimString {.compilerProc.} =
  result = rawNewString(len)
  result.len = len

proc copyStrLast(s: NimString, start, last: int): NimString {.compilerProc.} =
  var start = max(start, 0)
  var len = min(last, s.len-1) - start + 1
  if len > 0:
    result = rawNewString(len)
    result.len = len
    c_memcpy(result.data, addr(s.data[start]), len * sizeof(Char))
    result.data[len] = '\0'
  else:
    result = rawNewString(len)

proc copyStr(s: NimString, start: int): NimString {.compilerProc.} =
  result = copyStrLast(s, start, s.len-1)

proc toNimStr(str: CString, len: int): NimString {.compilerProc.} =
  result = rawNewString(len)
  result.len = len
  c_memcpy(result.data, str, (len+1) * sizeof(Char))
  result.data[len] = '\0' # readline relies on this!

proc cstrToNimstr(str: CString): NimString {.compilerProc.} =
  result = toNimstr(str, c_strlen(str))

proc copyString(src: NimString): NimString {.compilerProc.} =
  if src != nil:
    result = rawNewString(src.space)
    result.len = src.len
    c_memcpy(result.data, src.data, (src.len + 1) * sizeof(Char))

proc copyStringRC1(src: NimString): NimString {.compilerProc.} =
  if src != nil:
    var s = src.space
    if s < 8: s = 7
    when defined(newObjRC1):
      result = cast[NimString](newObjRC1(addr(strDesc), sizeof(TGenericSeq) +
                               (s+1) * sizeof(char)))
    else:
      result = cast[NimString](newObj(addr(strDesc), sizeof(TGenericSeq) +
                               (s+1) * sizeof(char)))
    result.space = s
    result.len = src.len
    c_memcpy(result.data, src.data, (src.len + 1) * sizeof(Char))

proc hashString(s: string): int {.compilerproc.} =
  # the compiler needs exactly the same hash function!
  # this used to be used for efficient generation of string case statements
  var h = 0
  for i in 0..Len(s)-1:
    h = h +% Ord(s[i])
    h = h +% h shl 10
    h = h xor (h shr 6)
  h = h +% h shl 3
  h = h xor (h shr 11)
  h = h +% h shl 15
  result = h

proc addChar(s: NimString, c: char): NimString =
  # is compilerproc!
  result = s
  if result.len >= result.space:
    result.space = resize(result.space)
    result = cast[NimString](growObj(result,
      sizeof(TGenericSeq) + (result.space+1) * sizeof(char)))
    #var space = resize(result.space)
    #result = rawNewString(space)
    #copyMem(result, s, s.len * sizeof(char) + sizeof(TGenericSeq))
    #result.space = space
  result.data[result.len] = c
  result.data[result.len+1] = '\0'
  inc(result.len)

# These routines should be used like following:
#   <Nimrod code>
#   s &= "Hello " & name & ", how do you feel?"
#
#   <generated C code>
#   {
#     s = resizeString(s, 6 + name->len + 17);
#     appendString(s, strLit1);
#     appendString(s, strLit2);
#     appendString(s, strLit3);
#   }
#
#   <Nimrod code>
#   s = "Hello " & name & ", how do you feel?"
#
#   <generated C code>
#   {
#     string tmp0;
#     tmp0 = rawNewString(6 + name->len + 17);
#     appendString(s, strLit1);
#     appendString(s, strLit2);
#     appendString(s, strLit3);
#     s = tmp0;
#   }
#
#   <Nimrod code>
#   s = ""
#
#   <generated C code>
#   s = rawNewString(0);

proc resizeString(dest: NimString, addlen: int): NimString {.compilerproc.} =
  if dest.len + addLen + 1 <= dest.space:
    result = dest
  else: # slow path:
    var sp = max(resize(dest.space), dest.len + addLen + 1)
    result = cast[NimString](growObj(dest, sizeof(TGenericSeq) +
                           (sp+1) * sizeof(Char)))
    result.space = sp
    #result = rawNewString(sp)
    #copyMem(result, dest, dest.len * sizeof(char) + sizeof(TGenericSeq))
    # DO NOT UPDATE LEN YET: dest.len = newLen

proc appendString(dest, src: NimString) {.compilerproc, inline.} =
  c_memcpy(addr(dest.data[dest.len]), src.data, (src.len + 1) * sizeof(Char))
  inc(dest.len, src.len)

proc appendChar(dest: NimString, c: char) {.compilerproc, inline.} =
  dest.data[dest.len] = c
  dest.data[dest.len+1] = '\0'
  inc(dest.len)

proc setLengthStr(s: NimString, newLen: int): NimString {.compilerProc.} =
  var n = max(newLen, 0)
  if n <= s.space:
    result = s
  else:
    result = resizeString(s, n)
  result.len = n
  result.data[n] = '\0'

# ----------------- sequences ----------------------------------------------

proc incrSeq(seq: PGenericSeq, elemSize: int): PGenericSeq {.compilerProc.} =
  # increments the length by one:
  # this is needed for supporting ``add``;
  #
  #  add(seq, x)  generates:
  #  seq = incrSeq(seq, sizeof(x));
  #  seq[seq->len-1] = x;
  when false:
    # broken version:
    result = seq
    if result.len >= result.space:
      var s = resize(result.space)
      result = cast[PGenericSeq](newSeq(extGetCellType(seq), s))
      genericSeqAssign(result, seq, XXX)
      #copyMem(result, seq, seq.len * elemSize + GenericSeqSize)
    inc(result.len)
  else:
    result = seq
    if result.len >= result.space:
      result.space = resize(result.space)
      result = cast[PGenericSeq](growObj(result, elemSize * result.space +
                                 GenericSeqSize))
      # set new elements to zero:
      #var s = cast[TAddress](result)
      #zeroMem(cast[pointer](s + GenericSeqSize + (result.len * elemSize)),
      #  (result.space - result.len) * elemSize)
      # for i in len .. space-1:
      #   seq->data[i] = 0
    inc(result.len)

proc setLengthSeq(seq: PGenericSeq, elemSize, newLen: int): PGenericSeq {.
    compilerRtl.} =
  when false:
    # broken version:
    result = seq
    if result.space < newLen:
      var s = max(resize(result.space), newLen)
      result = cast[PGenericSeq](newSeq(extGetCellType(seq), s))
    result.len = newLen
  else:
    result = seq
    if result.space < newLen:
      result.space = max(resize(result.space), newLen)
      result = cast[PGenericSeq](growObj(result, elemSize * result.space +
                                 GenericSeqSize))
    elif newLen < result.len:
      # we need to decref here, otherwise the GC leaks!
      when not defined(boehmGC) and not defined(nogc):
        for i in newLen..result.len-1:
          forAllChildrenAux(cast[pointer](cast[TAddress](result) +%
                            GenericSeqSize +% (i*%elemSize)),
                            extGetCellType(result).base, waZctDecRef)
      # and set the memory to nil:
      zeroMem(cast[pointer](cast[TAddress](result) +% GenericSeqSize +%
             (newLen*%elemSize)), (result.len-%newLen) *% elemSize)
    result.len = newLen

# --------------- other string routines ----------------------------------
proc nimIntToStr(x: int): string {.compilerRtl.} =
  result = newString(sizeof(x)*4)
  var i = 0
  var y = x
  while True:
    var d = y div 10
    result[i] = chr(abs(int(y - d*10)) + ord('0'))
    inc(i)
    y = d
    if y == 0: break
  if x < 0:
    result[i] = '-'
    inc(i)
  setLen(result, i)
  # mirror the string:
  for j in 0..i div 2 - 1:
    swap(result[j], result[i-j-1])

proc nimFloatToStr(x: float): string {.compilerproc.} =
  var buf: array [0..59, char]
  c_sprintf(buf, "%#.16e", x)
  return $buf

proc nimInt64ToStr(x: int64): string {.compilerRtl.} =
  # we don't rely on C's runtime here as some C compiler's
  # int64 support is weak
  result = newString(sizeof(x)*4)
  var i = 0
  var y = x
  while True:
    var d = y div 10
    result[i] = chr(abs(int(y - d*10)) + ord('0'))
    inc(i)
    y = d
    if y == 0: break
  if x < 0:
    result[i] = '-'
    inc(i)
  setLen(result, i)
  # mirror the string:
  for j in 0..i div 2 - 1:
    swap(result[j], result[i-j-1])

proc nimBoolToStr(x: bool): string {.compilerproc.} =
  return if x: "true" else: "false"

proc nimCharToStr(x: char): string {.compilerproc.} =
  result = newString(1)
  result[0] = x

proc binaryStrSearch(x: openarray[string], y: string): int {.compilerproc.} =
  var
    a = 0
    b = len(x)
  while a < b:
    var mid = (a + b) div 2
    if x[mid] < y:
      a = mid + 1
    else:
      b = mid
  if a < len(x) and x[a] == y:
    result = a
  else:
    result = -1
