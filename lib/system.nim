#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The compiler depends on the System module to work properly and the System
## module depends on the compiler. Most of the routines listed here use
## special compiler magic.
## Each module implicitly imports the System module; it must not be listed
## explicitly. Because of this there cannot be a user-defined module named
## ``system``.

type
  int* {.magic: Int.} ## default integer type; bitwidth depends on
                      ## architecture, but is always the same as a pointer
  int8* {.magic: Int8.} ## signed 8 bit integer type
  int16* {.magic: Int16.} ## signed 16 bit integer type
  int32* {.magic: Int32.} ## signed 32 bit integer type
  int64* {.magic: Int64.} ## signed 64 bit integer type
  uint* {.magic: UInt.} ## unsigned default integer type
  uint8* {.magic: UInt8.} ## unsigned 8 bit integer type
  uint16* {.magic: UInt16.} ## unsigned 16 bit integer type
  uint32* {.magic: UInt32.} ## unsigned 32 bit integer type
  uint64* {.magic: UInt64.} ## unsigned 64 bit integer type
  float* {.magic: Float.} ## default floating point type
  float32* {.magic: Float32.} ## 32 bit floating point type
  float64* {.magic: Float64.} ## 64 bit floating point type

type # we need to start a new type section here, so that ``0`` can have a type
  bool* {.magic: Bool.} = enum ## built-in boolean type
    false = 0, true = 1

const SystemModule = true

type
  char* {.magic: Char.} ## built-in 8 bit character type (unsigned)
  string* {.magic: String.} ## built-in string type
  cstring* {.magic: Cstring.} ## built-in cstring (*compatible string*) type
  pointer* {.magic: Pointer.} ## built-in pointer type, use the ``addr``
                              ## operator to get a pointer to a variable

const
  on* = true    ## alias for ``true``
  off* = false  ## alias for ``false``

{.push hints: off.}

type
  Ordinal* {.magic: Ordinal.}[T]
  `nil` {.magic: "Nil".}
  expr* {.magic: Expr.} ## meta type to denote an expression (for templates)
  stmt* {.magic: Stmt.} ## meta type to denote a statement (for templates)
  typeDesc* {.magic: TypeDesc.} ## meta type to denote a type description
  void* {.magic: "VoidType".}   ## meta type to denote the absense of any type
  auto* = expr
  any* = distinct auto

  TSignedInt* = int|int8|int16|int32|int64
    ## type class matching all signed integer types

  TUnsignedInt* = uint|uint8|uint16|uint32|uint64
    ## type class matching all unsigned integer types

  TInteger* = TSignedInt|TUnsignedInt
    ## type class matching all integer types

  TOrdinal* = int|int8|int16|int32|int64|bool|enum|uint8|uint16|uint32
    ## type class matching all ordinal types; however this includes enums with
    ## holes.
  
  TReal* = float|float32|float64
    ## type class matching all floating point number types

  TNumber* = TInteger|TReal
    ## type class matching all number types

proc defined*(x: expr): bool {.magic: "Defined", noSideEffect.}
  ## Special compile-time procedure that checks whether `x` is
  ## defined. `x` has to be an identifier or a qualified identifier.
  ## This can be used to check whether a library provides a certain
  ## feature or not:
  ##
  ## .. code-block:: Nimrod
  ##   when not defined(strutils.toUpper):
  ##     # provide our own toUpper proc here, because strutils is
  ##     # missing it.

when defined(useNimRtl):
  {.deadCodeElim: on.}

proc definedInScope*(x: expr): bool {.
  magic: "DefinedInScope", noSideEffect.}
  ## Special compile-time procedure that checks whether `x` is
  ## defined in the current scope. `x` has to be an identifier.

proc `not` *(x: bool): bool {.magic: "Not", noSideEffect.}
  ## Boolean not; returns true iff ``x == false``.

proc `and`*(x, y: bool): bool {.magic: "And", noSideEffect.}
  ## Boolean ``and``; returns true iff ``x == y == true``.
  ## Evaluation is lazy: if ``x`` is false,
  ## ``y`` will not even be evaluated.
proc `or`*(x, y: bool): bool {.magic: "Or", noSideEffect.}
  ## Boolean ``or``; returns true iff ``not (not x and not y)``.
  ## Evaluation is lazy: if ``x`` is true,
  ## ``y`` will not even be evaluated.
proc `xor`*(x, y: bool): bool {.magic: "Xor", noSideEffect.}
  ## Boolean `exclusive or`; returns true iff ``x != y``.

# for low and high the return type T may not be correct, but
# we handle that with compiler magic in SemLowHigh()
proc high*[T](x: T): T {.magic: "High", noSideEffect.}
  ## returns the highest possible index of an array, a sequence, a string or
  ## the highest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.

proc low*[T](x: T): T {.magic: "Low", noSideEffect.}
  ## returns the lowest possible index of an array, a sequence, a string or
  ## the lowest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.

type
  range*{.magic: "Range".}[T] ## Generic type to construct range types.
  array*{.magic: "Array".}[I, T]  ## Generic type to construct
                                  ## fixed-length arrays.
  openarray*{.magic: "OpenArray".}[T]  ## Generic type to construct open arrays.
                                       ## Open arrays are implemented as a
                                       ## pointer to the array data and a
                                       ## length field.
  varargs*{.magic: "Varargs".}[T] ## Generic type to construct a varargs type.
  set*{.magic: "Set".}[T]  ## Generic type to construct bit sets.

type
  TSlice* {.final, pure.}[T] = object ## builtin slice type
    a*, b*: T                         ## the bounds

proc `..`*[T](a, b: T): TSlice[T] {.noSideEffect, inline.} =
  ## `slice`:idx: operator that constructs an interval ``[a, b]``, both `a`
  ## and `b` are inclusive. Slices can also be used in the set constructor
  ## and in ordinal case statements, but then they are special-cased by the
  ## compiler.
  result.a = a
  result.b = b

proc `..`*[T](b: T): TSlice[T] {.noSideEffect, inline.} =
  ## `slice`:idx: operator that constructs an interval ``[default(T), b]``
  result.b = b

when not defined(niminheritable):
  {.pragma: inheritable.}

when not defined(JS) and not defined(NimrodVM):
  include "system/hti"

type
  Byte* = uInt8 ## this is an alias for ``uint8``, that is an unsigned
                ## int 8 bits wide.

  Natural* = range[0..high(int)]
    ## is an int type ranging from zero to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  Positive* = range[1..high(int)]
    ## is an int type ranging from one to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  TObject* {.exportc: "TNimObject", inheritable.} =
    object ## the root of Nimrod's object hierarchy. Objects should
           ## inherit from TObject or one of its descendants. However,
           ## objects that have no ancestor are allowed.

  TResult* = enum Failure, Success

proc sizeof*[T](x: T): natural {.magic: "SizeOf", noSideEffect.}
  ## returns the size of ``x`` in bytes. Since this is a low-level proc,
  ## its usage is discouraged - using ``new`` for the most cases suffices
  ## that one never needs to know ``x``'s size. As a special semantic rule,
  ## ``x`` may also be a type identifier (``sizeof(int)`` is valid).

proc `<`*[T](x: ordinal[T]): T {.magic: "UnaryLt", noSideEffect.}
  ## unary ``<`` that can be used for nice looking excluding ranges:
  ## 
  ## .. code-block:: nimrod
  ##   for i in 0 .. <10: echo i
  ##
  ## Semantically this is the same as ``pred``. 

proc succ*[T](x: ordinal[T], y = 1): T {.magic: "Succ", noSideEffect.}
  ## returns the ``y``-th successor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc pred*[T](x: ordinal[T], y = 1): T {.magic: "Pred", noSideEffect.}
  ## returns the ``y``-th predecessor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc inc*[T](x: var ordinal[T], y = 1) {.magic: "Inc", noSideEffect.}
  ## increments the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = succ(x, y)``.

proc dec*[T](x: var ordinal[T], y = 1) {.magic: "Dec", noSideEffect.}
  ## decrements the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = pred(x, y)``.
  
proc len*[TOpenArray: openArray|varargs](x: TOpenArray): int {.magic: "LengthOpenArray", noSideEffect.}
proc len*(x: cstring): int {.magic: "LengthStr", noSideEffect.}
proc len*[I, T](x: array[I, T]): int {.magic: "LengthArray", noSideEffect.}

# set routines:
proc incl*[T](x: var set[T], y: T) {.magic: "Incl", noSideEffect.}
  ## includes element ``y`` to the set ``x``. This is the same as
  ## ``x = x + {y}``, but it might be more efficient.

proc excl*[T](x: var set[T], y: T) {.magic: "Excl", noSideEffect.}
  ## excludes element ``y`` to the set ``x``. This is the same as
  ## ``x = x - {y}``, but it might be more efficient.

proc card*[T](x: set[T]): int {.magic: "Card", noSideEffect.}
  ## returns the cardinality of the set ``x``, i.e. the number of elements
  ## in the set.

proc ord*[T](x: T): int {.magic: "Ord", noSideEffect.}
  ## returns the internal int value of an ordinal value ``x``.

proc chr*(u: range[0..255]): char {.magic: "Chr", noSideEffect.}
  ## converts an int in the range 0..255 to a character.

# --------------------------------------------------------------------------
# built-in operators

proc ze*(x: int8): int {.magic: "Ze8ToI", noSideEffect.}
  ## zero extends a smaller integer type to ``int``. This treats `x` as
  ## unsigned.
proc ze*(x: int16): int {.magic: "Ze16ToI", noSideEffect.}
  ## zero extends a smaller integer type to ``int``. This treats `x` as
  ## unsigned.

proc ze64*(x: int8): int64 {.magic: "Ze8ToI64", noSideEffect.}
  ## zero extends a smaller integer type to ``int64``. This treats `x` as
  ## unsigned.
proc ze64*(x: int16): int64 {.magic: "Ze16ToI64", noSideEffect.}
  ## zero extends a smaller integer type to ``int64``. This treats `x` as
  ## unsigned.

proc ze64*(x: int32): int64 {.magic: "Ze32ToI64", noSideEffect.}
  ## zero extends a smaller integer type to ``int64``. This treats `x` as
  ## unsigned.
proc ze64*(x: int): int64 {.magic: "ZeIToI64", noDecl, noSideEffect.}
  ## zero extends a smaller integer type to ``int64``. This treats `x` as
  ## unsigned. Does nothing if the size of an ``int`` is the same as ``int64``.
  ## (This is the case on 64 bit processors.)

proc toU8*(x: int): int8 {.magic: "ToU8", noSideEffect.}
  ## treats `x` as unsigned and converts it to a byte by taking the last 8 bits
  ## from `x`.
proc toU16*(x: int): int16 {.magic: "ToU16", noSideEffect.}
  ## treats `x` as unsigned and converts it to an ``int16`` by taking the last
  ## 16 bits from `x`.
proc toU32*(x: int64): int32 {.magic: "ToU32", noSideEffect.}
  ## treats `x` as unsigned and converts it to an ``int32`` by taking the
  ## last 32 bits from `x`.


# integer calculations:
proc `+` *(x: int): int {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int8): int8 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int16): int16 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int32): int32 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int64): int64 {.magic: "UnaryPlusI64", noSideEffect.}
  ## Unary `+` operator for an integer. Has no effect.

proc `-` *(x: int): int {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int8): int8 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int16): int16 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int32): int32 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int64): int64 {.magic: "UnaryMinusI64", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

proc `not` *(x: int): int {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int8): int8 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int16): int16 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int32): int32 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int64): int64 {.magic: "BitnotI64", noSideEffect.}
  ## computes the `bitwise complement` of the integer `x`.

proc `+` *(x, y: int): int {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int8): int8 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int16): int16 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int32): int32 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int64): int64 {.magic: "AddI64", noSideEffect.}
  ## Binary `+` operator for an integer.

proc `-` *(x, y: int): int {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int8): int8 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int16): int16 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int32): int32 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int64): int64 {.magic: "SubI64", noSideEffect.}
  ## Binary `-` operator for an integer.

proc `*` *(x, y: int): int {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int8): int8 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int16): int16 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int32): int32 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int64): int64 {.magic: "MulI64", noSideEffect.}
  ## Binary `*` operator for an integer.

proc `div` *(x, y: int): int {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int8): int8 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int16): int16 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int32): int32 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int64): int64 {.magic: "DivI64", noSideEffect.}
  ## computes the integer division. This is roughly the same as
  ## ``floor(x/y)``.

proc `mod` *(x, y: int): int {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int8): int8 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int16): int16 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int32): int32 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int64): int64 {.magic: "ModI64", noSideEffect.}
  ## computes the integer modulo operation. This is the same as
  ## ``x - (x div y) * y``.

proc `shr` *(x, y: int): int {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int8): int8 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int16): int16 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int32): int32 {.magic: "ShrI", noSideEffect.}
proc `shr` *(x, y: int64): int64 {.magic: "ShrI64", noSideEffect.}
  ## computes the `shift right` operation of `x` and `y`.

proc `shl` *(x, y: int): int {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int8): int8 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int16): int16 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int32): int32 {.magic: "ShlI", noSideEffect.}
proc `shl` *(x, y: int64): int64 {.magic: "ShlI64", noSideEffect.}
  ## computes the `shift left` operation of `x` and `y`.

proc `and` *(x, y: int): int {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int8): int8 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int16): int16 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int32): int32 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int64): int64 {.magic: "BitandI64", noSideEffect.}
  ## computes the `bitwise and` of numbers `x` and `y`.

proc `or` *(x, y: int): int {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int8): int8 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int16): int16 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int32): int32 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int64): int64 {.magic: "BitorI64", noSideEffect.}
  ## computes the `bitwise or` of numbers `x` and `y`.

proc `xor` *(x, y: int): int {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int8): int8 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int16): int16 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int32): int32 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int64): int64 {.magic: "BitxorI64", noSideEffect.}
  ## computes the `bitwise xor` of numbers `x` and `y`.

proc `==` *(x, y: int): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int8): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int16): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int32): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int64): bool {.magic: "EqI64", noSideEffect.}
  ## Compares two integers for equality.

proc `<=` *(x, y: int): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int8): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int16): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int32): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int64): bool {.magic: "LeI64", noSideEffect.}
  ## Returns true iff `x` is less than or equal to `y`.

proc `<` *(x, y: int): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int8): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int16): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int32): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int64): bool {.magic: "LtI64", noSideEffect.}
  ## Returns true iff `x` is less than `y`.

proc abs*(x: int): int {.magic: "AbsI", noSideEffect.}
proc abs*(x: int8): int8 {.magic: "AbsI", noSideEffect.}
proc abs*(x: int16): int16 {.magic: "AbsI", noSideEffect.}
proc abs*(x: int32): int32 {.magic: "AbsI", noSideEffect.}
proc abs*(x: int64): int64 {.magic: "AbsI64", noSideEffect.}
  ## returns the absolute value of `x`. If `x` is ``low(x)`` (that 
  ## is -MININT for its type), an overflow exception is thrown (if overflow
  ## checking is turned on).

type
  IntMax32 = bool|int|int8|int16|int32

proc `+%` *(x, y: IntMax32): IntMax32 {.magic: "AddU", noSideEffect.}
proc `+%` *(x, y: Int64): Int64 {.magic: "AddU", noSideEffect.}
  ## treats `x` and `y` as unsigned and adds them. The result is truncated to
  ## fit into the result. This implements modulo arithmetic. No overflow
  ## errors are possible.

proc `-%` *(x, y: IntMax32): IntMax32 {.magic: "SubU", noSideEffect.}
proc `-%` *(x, y: Int64): Int64 {.magic: "SubU", noSideEffect.}
  ## treats `x` and `y` as unsigned and subtracts them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `*%` *(x, y: IntMax32): IntMax32 {.magic: "MulU", noSideEffect.}
proc `*%` *(x, y: Int64): Int64 {.magic: "MulU", noSideEffect.}
  ## treats `x` and `y` as unsigned and multiplies them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `/%` *(x, y: IntMax32): IntMax32 {.magic: "DivU", noSideEffect.}
proc `/%` *(x, y: Int64): Int64 {.magic: "DivU", noSideEffect.}
  ## treats `x` and `y` as unsigned and divides them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `%%` *(x, y: IntMax32): IntMax32 {.magic: "ModU", noSideEffect.}
proc `%%` *(x, y: Int64): Int64 {.magic: "ModU", noSideEffect.}
  ## treats `x` and `y` as unsigned and compute the modulo of `x` and `y`.
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic.
  ## No overflow errors are possible.
  
proc `<=%` *(x, y: IntMax32): bool {.magic: "LeU", noSideEffect.}
proc `<=%` *(x, y: Int64): bool {.magic: "LeU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) <= unsigned(y)``.

proc `<%` *(x, y: IntMax32): bool {.magic: "LtU", noSideEffect.}
proc `<%` *(x, y: Int64): bool {.magic: "LtU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) < unsigned(y)``.


# floating point operations:
proc `+` *(x: float): float {.magic: "UnaryPlusF64", noSideEffect.}
proc `-` *(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
proc `+` *(x, y: float): float {.magic: "AddF64", noSideEffect.}
proc `-` *(x, y: float): float {.magic: "SubF64", noSideEffect.}
proc `*` *(x, y: float): float {.magic: "MulF64", noSideEffect.}
proc `/` *(x, y: float): float {.magic: "DivF64", noSideEffect.}
  ## computes the floating point division

proc `==` *(x, y: float): bool {.magic: "EqF64", noSideEffect.}
proc `<=` *(x, y: float): bool {.magic: "LeF64", noSideEffect.}
proc `<`  *(x, y: float): bool {.magic: "LtF64", noSideEffect.}
proc abs*(x: float): float {.magic: "AbsF64", noSideEffect.}
proc min*(x, y: float): float {.magic: "MinF64", noSideEffect.}
proc max*(x, y: float): float {.magic: "MaxF64", noSideEffect.}

# set operators
proc `*` *[T](x, y: set[T]): set[T] {.magic: "MulSet", noSideEffect.}
  ## This operator computes the intersection of two sets.
proc `+` *[T](x, y: set[T]): set[T] {.magic: "PlusSet", noSideEffect.}
  ## This operator computes the union of two sets.
proc `-` *[T](x, y: set[T]): set[T] {.magic: "MinusSet", noSideEffect.}
  ## This operator computes the difference of two sets.
proc `-+-` *[T](x, y: set[T]): set[T] {.magic: "SymDiffSet", noSideEffect.}
  ## computes the symmetric set difference. This is the same as
  ## ``(A - B) + (B - A)``, but more efficient.

# comparison operators:
proc `==` *[TEnum: enum](x, y: TEnum): bool {.magic: "EqEnum", noSideEffect.}
proc `==` *(x, y: pointer): bool {.magic: "EqRef", noSideEffect.}
proc `==` *(x, y: cstring): bool {.magic: "EqCString", noSideEffect.}
proc `==` *(x, y: char): bool {.magic: "EqCh", noSideEffect.}
proc `==` *(x, y: bool): bool {.magic: "EqB", noSideEffect.}
proc `==` *[T](x, y: set[T]): bool {.magic: "EqSet", noSideEffect.}
proc `==` *[T](x, y: ptr T): bool {.magic: "EqRef", noSideEffect.}
proc `==` *[T: proc](x, y: T): bool {.magic: "EqProc", noSideEffect.}

proc `<=` *[TEnum: enum](x, y: TEnum): bool {.magic: "LeEnum", noSideEffect.}
proc `<=` *(x, y: char): bool {.magic: "LeCh", noSideEffect.}
proc `<=` *[T](x, y: set[T]): bool {.magic: "LeSet", noSideEffect.}
proc `<=` *(x, y: bool): bool {.magic: "LeB", noSideEffect.}
proc `<=` *(x, y: pointer): bool {.magic: "LePtr", noSideEffect.}

proc `<` *[TEnum: enum](x, y: TEnum): bool {.magic: "LtEnum", noSideEffect.}
proc `<` *(x, y: char): bool {.magic: "LtCh", noSideEffect.}
proc `<` *[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
proc `<` *(x, y: bool): bool {.magic: "LtB", noSideEffect.}
proc `<` *[T](x, y: ptr T): bool {.magic: "LtPtr", noSideEffect.}
proc `<` *(x, y: pointer): bool {.magic: "LtPtr", noSideEffect.}

template `!=` * (x, y: expr): expr {.immediate.} =
  ## unequals operator. This is a shorthand for ``not (x == y)``.
  not (x == y)

template `>=` * (x, y: expr): expr {.immediate.} =
  ## "is greater or equals" operator. This is the same as ``y <= x``.
  y <= x

template `>` * (x, y: expr): expr {.immediate.} =
  ## "is greater" operator. This is the same as ``y < x``.
  y < x

proc contains*[T](x: set[T], y: T): bool {.magic: "InSet", noSideEffect.}
  ## One should overload this proc if one wants to overload the ``in`` operator.
  ## The parameters are in reverse order! ``a in b`` is a template for
  ## ``contains(b, a)``.
  ## This is because the unification algorithm that Nimrod uses for overload
  ## resolution works from left to right.
  ## But for the ``in`` operator that would be the wrong direction for this
  ## piece of code:
  ##
  ## .. code-block:: Nimrod
  ##   var s: set[range['a'..'z']] = {'a'..'c'}
  ##   writeln(stdout, 'b' in s)
  ##
  ## If ``in`` had been declared as ``[T](elem: T, s: set[T])`` then ``T`` would
  ## have been bound to ``char``. But ``s`` is not compatible to type
  ## ``set[char]``! The solution is to bind ``T`` to ``range['a'..'z']``. This
  ## is achieved by reversing the parameters for ``contains``; ``in`` then
  ## passes its arguments in reverse order.

proc contains*[T](s: TSlice[T], value: T): bool {.noSideEffect, inline.} =
  result = s.a <= value and value <= s.b

template `in` * (x, y: expr): expr {.immediate.} = contains(y, x)
template `not_in` * (x, y: expr): expr {.immediate.} = not contains(y, x)

proc `is` *[T, S](x: T, y: S): bool {.magic: "Is", noSideEffect.}
template `is_not` *(x, y: expr): expr {.immediate.} = not (x is y)

proc `of` *[T, S](x: T, y: S): bool {.magic: "Of", noSideEffect.}

proc cmp*[T](x, y: T): int {.procvar.} =
  ## Generic compare proc. Returns a value < 0 iff x < y, a value > 0 iff x > y
  ## and 0 iff x == y. This is useful for writing generic algorithms without
  ## performance loss. This generic implementation uses the `==` and `<`
  ## operators.
  if x == y: return 0
  if x < y: return -1
  return 1

type
  TEndian* = enum ## is a type describing the endianness of a processor.
    littleEndian, bigEndian

const
  isMainModule* {.magic: "IsMainModule".}: bool = false
    ## is true only when accessed in the main module. This works thanks to
    ## compiler magic. It is useful to embed testing code in a module.

  CompileDate* {.magic: "CompileDate"}: string = "0000-00-00"
    ## is the date of compilation as a string of the form
    ## ``YYYY-MM-DD``. This works thanks to compiler magic.

  CompileTime* {.magic: "CompileTime"}: string = "00:00:00"
    ## is the time of compilation as a string of the form
    ## ``HH:MM:SS``. This works thanks to compiler magic.

  NimrodVersion* {.magic: "NimrodVersion"}: string = "0.0.0"
    ## is the version of Nimrod as a string.
    ## This works thanks to compiler magic.

  NimrodMajor* {.magic: "NimrodMajor"}: int = 0
    ## is the major number of Nimrod's version.
    ## This works thanks to compiler magic.

  NimrodMinor* {.magic: "NimrodMinor"}: int = 0
    ## is the minor number of Nimrod's version.
    ## This works thanks to compiler magic.

  NimrodPatch* {.magic: "NimrodPatch"}: int = 0
    ## is the patch number of Nimrod's version.
    ## This works thanks to compiler magic.

  cpuEndian* {.magic: "CpuEndian"}: TEndian = littleEndian
    ## is the endianness of the target CPU. This is a valuable piece of
    ## information for low-level code only. This works thanks to compiler
    ## magic.

  hostOS* {.magic: "HostOS"}: string = ""
    ## a string that describes the host operating system. Possible values:
    ## "windows", "macosx", "linux", "netbsd", "freebsd", "openbsd", "solaris",
    ## "aix", "standalone".
        
  hostCPU* {.magic: "HostCPU"}: string = ""
    ## a string that describes the host CPU. Possible values:
    ## "i386", "alpha", "powerpc", "sparc", "amd64", "mips", "arm".
  
  appType* {.magic: "AppType"}: string = ""
    ## a string that describes the application type. Possible values:
    ## "console", "gui", "lib".
  
  seqShallowFlag = 1 shl (sizeof(int)*8-1)

proc compileOption*(option: string): bool {.
  magic: "CompileOption", noSideEffect.}
  ## can be used to determine an on|off compile-time option. Example:
  ##
  ## .. code-block:: nimrod
  ##   when compileOption("floatchecks"): 
  ##     echo "compiled with floating point NaN and Inf checks"
  
proc compileOption*(option, arg: string): bool {.
  magic: "CompileOptionArg", noSideEffect.}
  ## can be used to determine an enum compile-time option. Example:
  ##
  ## .. code-block:: nimrod
  ##   when compileOption("opt", "size") and compileOption("gc", "boehm"): 
  ##     echo "compiled with optimization for size and uses Boehm's GC"
    
const
  hasThreadSupport = compileOption("threads")
  hasSharedHeap = defined(boehmgc) # don't share heaps; every thread has its own

# --- Strings ---
proc `==` *(x, y: string): bool {.magic: "EqStr", noSideEffect.}
proc `<` *(x, y: string): bool {.magic: "LtStr", noSideEffect.}
proc `<=` *(x, y: string): bool {.magic: "LeStr", noSideEffect.}

proc cmp*(x, y: string): int {.noSideEffect, procvar.}
  ## Compare proc for strings. More efficient than the generic version.


# --- Exceptions ---

type
  E_Base* {.compilerproc.} = object of TObject ## base exception class;
                                               ## each exception has to
                                               ## inherit from `E_Base`.
    parent: ref E_Base        ## parent exception (can be used as a stack)
    name: cstring             ## The exception's name is its Nimrod identifier.
                              ## This field is filled automatically in the
                              ## ``raise`` statement.
    msg* {.exportc: "message".}: string ## the exception's message. Not
                                        ## providing an exception message 
                                        ## is bad style.
    trace: string

  EAsynch* = object of E_Base ## Abstract exception class for
                              ## *asynchronous exceptions* (interrupts).
                              ## This is rarely needed: Most
                              ## exception types inherit from `ESynch`
  ESynch* = object of E_Base  ## Abstract exception class for
                              ## *synchronous exceptions*. Most exceptions
                              ## should be inherited (directly or indirectly)
                              ## from ESynch.
  ESystem* = object of ESynch ## Abstract class for exceptions that the runtime
                              ## system raises.
  EIO* = object of ESystem    ## raised if an IO error occured.
  EOS* = object of ESystem    ## raised if an operating system service failed.
  EInvalidLibrary* = object of EOS ## raised if a dynamic library
                                   ## could not be loaded.
  EResourceExhausted* = object of ESystem ## raised if a resource request
                                          ## could not be fullfilled.
  EArithmetic* = object of ESynch       ## raised if any kind of arithmetic
                                        ## error occured.
  EDivByZero* {.compilerproc.} =
    object of EArithmetic ## is the exception class for integer divide-by-zero
                          ## errors.
  EOverflow* {.compilerproc.} =
    object of EArithmetic  ## is the exception class for integer calculations
                           ## whose results are too large to fit in the
                           ## provided bits.

  EAccessViolation* {.compilerproc.} =
    object of ESynch ## the exception class for invalid memory access errors

  EAssertionFailed* {.compilerproc.} =
    object of ESynch  ## is the exception class for Assert
                      ## procedures that is raised if the
                      ## assertion proves wrong

  EControlC* = object of EAsynch        ## is the exception class for Ctrl+C
                                        ## key presses in console applications.

  EInvalidValue* = object of ESynch     ## is the exception class for string
                                        ## and object conversion errors.
  EInvalidKey* = object of EInvalidValue ## is the exception class if a key
                                         ## cannot be found in a table.

  EOutOfMemory* = object of ESystem     ## is the exception class for
                                        ## unsuccessful attempts to allocate
                                        ## memory.

  EInvalidIndex* = object of ESynch     ## is raised if an array index is out
                                        ## of bounds.
  EInvalidField* = object of ESynch     ## is raised if a record field is not
                                        ## accessible because its dicriminant's
                                        ## value does not fit.

  EOutOfRange* = object of ESynch       ## is raised if a range check error
                                        ## occured.

  EStackOverflow* = object of ESystem   ## is raised if the hardware stack
                                        ## used for subroutine calls overflowed.

  ENoExceptionToReraise* = object of ESynch ## is raised if there is no
                                            ## exception to reraise.

  EInvalidObjectAssignment* =
    object of ESynch ## is raised if an object gets assigned to its
                     ## parent's object.

  EInvalidObjectConversion* =
    object of ESynch ## is raised if an object is converted to an incompatible
                     ## object type.

  EFloatingPoint* = object of ESynch ## base class for floating point exceptions
  EFloatInvalidOp* {.compilerproc.} = 
    object of EFloatingPoint ## Invalid operation according to IEEE: Raised by 
                             ## 0.0/0.0, for example.
  EFloatDivByZero* {.compilerproc.} = 
    object of EFloatingPoint ## Division by zero. Divisor is zero and dividend 
                             ## is a finite nonzero number.
  EFloatOverflow* {.compilerproc.} = 
    object of EFloatingPoint ## Overflow. Operation produces a result 
                             ## that exceeds the range of the exponent
  EFloatUnderflow* {.compilerproc.} = 
    object of EFloatingPoint ## Underflow. Operation produces a result 
                             ## that is too small to be represented as 
                             ## a normal number
  EFloatInexact* {.compilerproc.} = 
    object of EFloatingPoint ## Inexact. Operation produces a result
                             ## that cannot be represented with infinite
                             ## precision -- for example, 2.0 / 3.0, log(1.1) 
                             ## NOTE: Nimrod currently does not detect these!
  EDeadThread* =
    object of ESynch ## is raised if it is attempted to send a message to a
                     ## dead thread.
                     
include "sys/effects"
when not defined(noDynamicAlloc):
  # Mutating strings
  include "sys/strings"
  include "sys/ref"
  include "sys/exceptions"
else:
  {.warn: "define newException".}

when defined(profiler):
  proc nimProfile() {.compilerProc, noinline.}
when hasThreadSupport:
  {.pragma: rtlThreadVar, threadvar.}
else:
  {.pragma: rtlThreadVar.}

const
  QuitSuccess* = 0
    ## is the value that should be passed to ``quit`` to indicate
    ## success.

  QuitFailure* = 1
    ## is the value that should be passed to ``quit`` to indicate
    ## failure.

var programResult* {.exportc: "nim_program_result".}: int
  ## modify this varialbe to specify the exit code of the program
  ## under normal circumstances. When the program is terminated
  ## prematurelly using ``quit``, this value is ignored.

proc quit*(errorcode: int = QuitSuccess) {.
  magic: "Exit", importc: "exit", noDecl, noReturn.}
  ## Stops the program immediately with an exit code.
  ##
  ## Before stopping the program the "quit procedures" are called in the
  ## opposite order they were added with ``addQuitProc``. ``quit`` never
  ## returns and ignores any exception that may have been raised by the quit
  ## procedures.  It does *not* call the garbage collector to free all the
  ## memory, unless a quit procedure calls ``GC_collect``.
  ##
  ## The proc ``quit(QuitSuccess)`` is called implicitly when your nimrod
  ## program finishes without incident. A raised unhandled exception is
  ## equivalent to calling ``quit(QuitFailure)``.

include "system/inclrtl"

when not defined(JS) and not defined(nimrodVm) and hostOS != "standalone":
  include "system/cgprocs"

type
  TAddress* = int
    ## is the signed integer type that should be used for converting
    ## pointers to integer addresses for readability.

  BiggestInt* = int64
    ## is an alias for the biggest signed integer type the Nimrod compiler
    ## supports. Currently this is ``int64``, but it is platform-dependant
    ## in general.

  BiggestFloat* = float64
    ## is an alias for the biggest floating point type the Nimrod
    ## compiler supports. Currently this is ``float64``, but it is
    ## platform-dependant in general.

type # these work for most platforms:
  cchar* {.importc: "char", nodecl.} = char
    ## This is the same as the type ``char`` in *C*.
  cschar* {.importc: "signed char", nodecl.} = int8
    ## This is the same as the type ``signed char`` in *C*.
  cshort* {.importc: "short", nodecl.} = int16
    ## This is the same as the type ``short`` in *C*.
  cint* {.importc: "int", nodecl.} = int32
    ## This is the same as the type ``int`` in *C*.
  csize* {.importc: "size_t", nodecl.} = int
    ## This is the same as the type ``size_t`` in *C*.
  clong* {.importc: "long", nodecl.} = int
    ## This is the same as the type ``long`` in *C*.
  clonglong* {.importc: "long long", nodecl.} = int64
    ## This is the same as the type ``long long`` in *C*.
  cfloat* {.importc: "float", nodecl.} = float32
    ## This is the same as the type ``float`` in *C*.
  cdouble* {.importc: "double", nodecl.} = float64
    ## This is the same as the type ``double`` in *C*.
  clongdouble* {.importc: "long double", nodecl.} = BiggestFloat
    ## This is the same as the type ``long double`` in *C*.
    ## This C type is not supported by Nimrod's code generator

  cuchar* {.importc: "unsigned char", nodecl.} = char
    ## This is the same as the type ``unsigned char`` in *C*.
  cushort* {.importc: "unsigned short", nodecl.} = uint16
    ## This is the same as the type ``unsigned short`` in *C*.
  cuint* {.importc: "int", nodecl.} = uint32
    ## This is the same as the type ``unsigned int`` in *C*.
  culong* {.importc: "unsigned long", nodecl.} = uint
    ## This is the same as the type ``unsigned long`` in *C*.
  culonglong* {.importc: "unsigned long long", nodecl.} = uint64
    ## This is the same as the type ``unsigned long long`` in *C*.

  cstringArray* {.importc: "char**", nodecl.} = ptr array [0..50_000, cstring]
    ## This is binary compatible to the type ``char**`` in *C*. The array's
    ## high value is large enough to disable bounds checking in practice.
    ## Use `cstringArrayToSeq` to convert it into a ``seq[string]``.
  
  PFloat32* = ptr Float32 ## an alias for ``ptr float32``
  PFloat64* = ptr Float64 ## an alias for ``ptr float64``
  PInt64* = ptr Int64 ## an alias for ``ptr int64``
  PInt32* = ptr Int32 ## an alias for ``ptr int32``

proc toFloat*(i: int): float {.
  magic: "ToFloat", noSideEffect, importc: "toFloat".}
  ## converts an integer `i` into a ``float``. If the conversion
  ## fails, `EInvalidValue` is raised. However, on most platforms the
  ## conversion cannot fail.

proc toBiggestFloat*(i: biggestint): biggestfloat {.
  magic: "ToBiggestFloat", noSideEffect, importc: "toBiggestFloat".}
  ## converts an biggestint `i` into a ``biggestfloat``. If the conversion
  ## fails, `EInvalidValue` is raised. However, on most platforms the
  ## conversion cannot fail.

proc toInt*(f: float): int {.
  magic: "ToInt", noSideEffect, importc: "toInt".}
  ## converts a floating point number `f` into an ``int``. Conversion
  ## rounds `f` if it does not contain an integer value. If the conversion
  ## fails (because `f` is infinite for example), `EInvalidValue` is raised.

proc toBiggestInt*(f: biggestfloat): biggestint {.
  magic: "ToBiggestInt", noSideEffect, importc: "toBiggestInt".}
  ## converts a biggestfloat `f` into a ``biggestint``. Conversion
  ## rounds `f` if it does not contain an integer value. If the conversion
  ## fails (because `f` is infinite for example), `EInvalidValue` is raised.

proc addQuitProc*(QuitProc: proc() {.noconv.}) {.importc: "atexit", nodecl.}
  ## adds/registers a quit procedure. Each call to ``addQuitProc``
  ## registers another quit procedure. Up to 30 procedures can be
  ## registered. They are executed on a last-in, first-out basis
  ## (that is, the last function registered is the first to be executed).
  ## ``addQuitProc`` raises an EOutOfIndex if ``quitProc`` cannot be
  ## registered.

# Support for addQuitProc() is done by Ansi C's facilities here.
# In case of an unhandled exeption the exit handlers should
# not be called explicitly! The user may decide to do this manually though.

proc swap*[T](a, b: var T) {.magic: "Swap", noSideEffect.}
  ## swaps the values `a` and `b`. This is often more efficient than
  ## ``tmp = a; a = b; b = tmp``. Particularly useful for sorting algorithms.

template `>=%` *(x, y: expr): expr {.immediate.} = y <=% x
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) >= unsigned(y)``.

template `>%` *(x, y: expr): expr {.immediate.} = y <% x
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) > unsigned(y)``.

# new constants:
const
  inf* {.magic: "Inf".} = 1.0 / 0.0
    ## contains the IEEE floating point value of positive infinity.
  neginf* {.magic: "NegInf".} = -inf
    ## contains the IEEE floating point value of negative infinity.
  nan* {.magic: "NaN".} = 0.0 / 0.0
    ## contains an IEEE floating point value of *Not A Number*. Note
    ## that you cannot compare a floating point value to this value
    ## and expect a reasonable result - use the `classify` procedure
    ## in the module ``math`` for checking for NaN.

iterator countdown*[T](a, b: T, step = 1): T {.inline.} =
  ## Counts from ordinal value `a` down to `b` with the given
  ## step count. `T` may be any ordinal type, `step` may only
  ## be positive.
  var res = a
  while res >= b:
    yield res
    dec(res, step)

iterator countup*[S, T](a: S, b: T, step = 1): T {.inline.} =
  ## Counts from ordinal value `a` up to `b` with the given
  ## step count. `S`, `T` may be any ordinal type, `step` may only
  ## be positive.
  var res: T = T(a)
  while res <= b:
    yield res
    inc(res, step)

iterator `..`*[S, T](a: S, b: T): T {.inline.} =
  ## An alias for `countup`.
  var res: T = T(a)
  while res <= b:
    yield res
    inc res

iterator `||`*[S, T](a: S, b: T, annotation=""): T {.
  inline, magic: "OmpParFor", sideEffect.} =
  ## parallel loop iterator. Same as `..` but the loop may run in parallel.
  ## `annotation` is an additional annotation for the code generator to use.
  ## Note that the compiler maps that to
  ## the ``#pragma omp parallel for`` construct of `OpenMP`:idx: and as
  ## such isn't aware of the parallelism in your code! Be careful! Later
  ## versions of ``||`` will get proper support by Nimrod's code generator
  ## and GC.
  nil

proc min*(x, y: int): int {.magic: "MinI", noSideEffect.}
proc min*(x, y: int8): int8 {.magic: "MinI", noSideEffect.}
proc min*(x, y: int16): int16 {.magic: "MinI", noSideEffect.}
proc min*(x, y: int32): int32 {.magic: "MinI", noSideEffect.}
proc min*(x, y: int64): int64 {.magic: "MinI64", noSideEffect.}
  ## The minimum value of two integers.

proc min*[T](x: varargs[T]): T =
  ## The minimum value of `x`. ``T`` needs to have a ``<`` operator.
  result = x[0]
  for i in 1..high(x):
    if x[i] < result: result = x[i]

proc max*(x, y: int): int {.magic: "MaxI", noSideEffect.}
proc max*(x, y: int8): int8 {.magic: "MaxI", noSideEffect.}
proc max*(x, y: int16): int16 {.magic: "MaxI", noSideEffect.}
proc max*(x, y: int32): int32 {.magic: "MaxI", noSideEffect.}
proc max*(x, y: int64): int64 {.magic: "MaxI64", noSideEffect.}
  ## The maximum value of two integers.

proc max*[T](x: varargs[T]): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  result = x[0]
  for i in 1..high(x):
    if result < x[i]: result = x[i]

proc clamp*[T](x, a, b: T): T =
  ## limits the value ``x`` within the interval [a, b] 
  if x < a: return a
  if x > b: return b
  return x

iterator items*[T](a: openarray[T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while i < len(a):
    yield a[i]
    inc(i)

iterator items*[IX, T](a: array[IX, T]): T {.inline.} =
  ## iterates over each item of `a`.
  var i = low(IX)
  if i <= high(IX):
    while true:
      yield a[i]
      if i >= high(IX): break
      inc(i)

iterator items*[T](a: set[T]): T {.inline.} =
  ## iterates over each element of `a`. `items` iterates only over the
  ## elements that are really in the set (and not over the ones the set is
  ## able to hold).
  var i = low(T)
  if i <= high(T):
    while true:
      if i in a: yield i
      if i >= high(T): break
      inc(i)

iterator items*(a: cstring): char {.inline.} =
  ## iterates over each item of `a`.
  var i = 0
  while a[i] != '\0':
    yield a[i]
    inc(i)

iterator items*(E: typedesc[enum]): E =
  ## iterates over the values of the enum ``E``.
  for v in low(E)..high(E):
    yield v

iterator pairs*[T](a: openarray[T]): tuple[key: int, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc(i)

iterator pairs*[IX, T](a: array[IX, T]): tuple[key: IX, val: T] {.inline.} =
  ## iterates over each item of `a`. Yields ``(index, a[index])`` pairs.
  var i = low(IX)
  if i <= high(IX):
    while true:
      yield (i, a[i])
      if i >= high(IX): break
      inc(i)

proc isNil*[T](x: ptr T): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: pointer): bool {.noSideEffect, magic: "IsNil".}
proc isNil*(x: cstring): bool {.noSideEffect, magic: "IsNil".}
proc isNil*[T: proc](x: T): bool {.noSideEffect, magic: "IsNil".}
  ## Fast check whether `x` is nil. This is sometimes more efficient than
  ## ``== nil``.
proc isNil*[T](x: ref T): bool {.noSideEffect, magic: "IsNil".}

proc find*[T, S: typeDesc](a: T, item: S): int {.inline.}=
  ## Returns the first index of `item` in `a` or -1 if not found. This requires
  ## appropriate `items` and `==` operations to work.
  for i in items(a):
    if i == item: return
    inc(result)
  result = -1

proc contains*[T](a: openArray[T], item: T): bool {.inline.}=
  ## Returns true if `item` is in `a` or false if not found. This is a shortcut
  ## for ``find(a, item) >= 0``.
  return find(a, item) >= 0

iterator fields*[T: tuple|object](x: T): TObject {.
  magic: "Fields", noSideEffect.}
  ## iterates over every field of `x`. Warning: This really transforms
  ## the 'for' and unrolls the loop. The current implementation also has a bug
  ## that affects symbol binding in the loop body.
iterator fields*[S:tuple|object, T:tuple|object](x: S, y: T): tuple[a,b: expr] {.
  magic: "Fields", noSideEffect.}
  ## iterates over every field of `x` and `y`.
  ## Warning: This is really transforms the 'for' and unrolls the loop. 
  ## The current implementation also has a bug that affects symbol binding
  ## in the loop body.
iterator fieldPairs*[T: tuple|object](x: T): TObject {.
  magic: "FieldPairs", noSideEffect.}
  ## iterates over every field of `x`. Warning: This really transforms
  ## the 'for' and unrolls the loop. The current implementation also has a bug
  ## that affects symbol binding in the loop body.
iterator fieldPairs*[S: tuple|object, T: tuple|object](x: S, y: T): tuple[
  a, b: expr] {.
  magic: "FieldPairs", noSideEffect.}
  ## iterates over every field of `x` and `y`.
  ## Warning: This really transforms the 'for' and unrolls the loop. 
  ## The current implementation also has a bug that affects symbol binding
  ## in the loop body.

proc `==`*[T: tuple|object](x, y: T): bool = 
  ## generic ``==`` operator for tuples that is lifted from the components
  ## of `x` and `y`.
  for a, b in fields(x, y):
    if a != b: return false
  return true

proc `<=`*[T: tuple](x, y: T): bool = 
  ## generic ``<=`` operator for tuples that is lifted from the components
  ## of `x` and `y`. This implementation uses `cmp`.
  for a, b in fields(x, y):
    var c = cmp(a, b)
    if c < 0: return true
    if c > 0: return false
  return true

proc `<`*[T: tuple](x, y: T): bool = 
  ## generic ``<`` operator for tuples that is lifted from the components
  ## of `x` and `y`. This implementation uses `cmp`.
  for a, b in fields(x, y):
    var c = cmp(a, b)
    if c < 0: return true
    if c > 0: return false
  return false

template sysAssert(cond: bool, msg: string) =
  when defined(useSysAssert):
    if not cond:
      echo "[SYSASSERT] ", msg
      quit 1
  nil

type
  PFrame* = ptr TFrame  ## represents a runtime frame of the call stack;
                        ## part of the debugger API.
  TFrame* {.importc, nodecl, final.} = object ## the frame itself
    prev*: PFrame       ## previous frame; used for chaining the call stack
    procname*: cstring  ## name of the proc that is currently executing
    line*: int          ## line number of the proc that is currently executing
    filename*: cstring  ## filename of the proc that is currently executing
    len*: int           ## length of the inspectable slots

proc getTypeInfo*[T](x: T): pointer {.magic: "GetTypeInfo".}
  ## get type information for `x`. Ordinary code should not use this, but
  ## the `typeinfo` module instead.

when not defined(noDynamicAlloc):
  include "sys/ref2"

when not defined(JS): #and not defined(NimrodVM):
  {.push stack_trace: off, profiler:off.}

  when not defined(NimrodVM):
    proc initGC()
    proc initStackBottom() {.inline, compilerproc.} =
      # WARNING: This is very fragile! An array size of 8 does not work on my
      # Linux 64bit system. -- That's because the stack direction is the other
      # way round.
      when defined(setStackBottom):
        var locals {.volatile.}: pointer
        locals = addr(locals)
        setStackBottom(locals)

  include "system/ansi_c"

  when not defined(NimrodVM) and defined(endb):
    proc endbStep()

  # -------------------------------------------------------------------------

  type
    PSafePoint = ptr TSafePoint
    TSafePoint {.compilerproc, final.} = object
      prev: PSafePoint # points to next safe point ON THE STACK
      status: int
      context: C_JmpBuf
      hasRaiseAction: bool
      raiseAction: proc (e: ref E_Base): bool {.closure.}
  
  # Let's hope this just works.
  when defined(initAllocator):
    initAllocator()
  when hasThreadSupport:
    include "system/syslocks"
    include "system/threads"
  elif not defined(nogc) and not defined(NimrodVM):
    when not defined(useNimRtl) and not defined(createNimRtl): initStackBottom()
    initGC()

  when not defined(NimrodVM) and hostOS != "standalone":
    proc setControlCHook*(hook: proc () {.noconv.})
      ## allows you to override the behaviour of your application when CTRL+C
      ## is pressed. Only one such hook is supported.
    proc writeStackTrace*() {.tags: [FWriteIO].}
      ## writes the current stack trace to ``stderr``. This is only works
      ## for debug builds.
    proc getStackTrace*(): string
      ## gets the current stack trace. This only works for debug builds.
    proc getStackTrace*(e: ref E_Base): string
      ## gets the stack trace associated with `e`, which is the stack that
      ## lead to the ``raise`` statement. This only works for debug builds.
        
  {.push stack_trace: off, profiler:off.}
  when hostOS == "standalone":
    include "system/embedded"
      
  # we cannot compile this with stack tracing on
  # as it would recurse endlessly!
  include "system/arithm"
  {.pop.} # stack trace

  when not(defined(noIO) or defined(noDynamicAlloc) or hostOS == "standalone"):
    include "sys/io.nim"
      
  when hostOS != "standalone" and not defined(NimrodVM):
    include "system/dyncalls"
  when not defined(NimrodVM):
    include "system/sets"

    const
      GenericSeqSize = (2 * sizeof(int))
      
    proc getDiscriminant(aa: Pointer, n: ptr TNimNode): int =
      sysAssert(n.kind == nkCase, "getDiscriminant: node != nkCase")
      var d: int
      var a = cast[TAddress](aa)
      case n.typ.size
      of 1: d = ze(cast[ptr int8](a +% n.offset)[])
      of 2: d = ze(cast[ptr int16](a +% n.offset)[])
      of 4: d = int(cast[ptr int32](a +% n.offset)[])
      else: sysAssert(false, "getDiscriminant: invalid n.typ.size")
      return d

    proc selectBranch(aa: Pointer, n: ptr TNimNode): ptr TNimNode =
      var discr = getDiscriminant(aa, n)
      if discr <% n.len:
        result = n.sons[discr]
        if result == nil: result = n.sons[n.len]
        # n.sons[n.len] contains the ``else`` part (but may be nil)
      else:
        result = n.sons[n.len]

    include "sys/mem"
    include "system/mmdisp"
    {.push stack_trace: off, profiler:off.}
    when hostOS != "standalone": include "system/sysstr"
    proc add*(x: var string, y: cstring) {.noStackFrame.} =
      var i = 0
      while y[i] != '\0':
        add(x, y[i])
        inc(i)
    {.pop.}

    when hasThreadSupport:
      include "system/channels"

  when hostOS != "standalone" and not defined(NimrodVM):
    {.push stack_trace: off, profiler:off.}
    include "system/excpt"
    {.pop.}

    include "system/assign"
    include "system/repr"

  {.push stack_trace: off, profiler:off.}
  when defined(endb) and not defined(NimrodVM):
    include "system/debugger"

  when defined(profiler) or defined(memProfiler):
    include "system/profiler"
  {.pop.} # stacktrace

  # --- Ansi_C dependent string stuff ---
  
  proc cstringArrayToSeq*(a: cstringArray, len: int): seq[string] =
    ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
    ## of length ``len``.
    newSeq(result, len)
    for i in 0..len-1: result[i] = $a[i]

  proc cstringArrayToSeq*(a: cstringArray): seq[string] =
    ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
    ## terminated by ``nil``.
    var L = 0
    while a[L] != nil: inc(L)
    result = cstringArrayToSeq(a, L)

  when not defined(NimrodVM):
    proc allocCStringArray*(a: openArray[string]): cstringArray =
      ## creates a NULL terminated cstringArray from `a`. The result has to
      ## be freed with `deallocCStringArray` after it's not needed anymore.
      result = cast[cstringArray](alloc0((a.len+1) * sizeof(cstring)))
      for i in 0 .. a.high:
        # XXX get rid of this string copy here:
        var x = a[i]
        result[i] = cast[cstring](alloc0(x.len+1))
        copyMem(result[i], addr(x[0]), x.len)

    proc deallocCStringArray*(a: cstringArray) =
      ## frees a NULL terminated cstringArray.
      var i = 0
      while a[i] != nil:
        dealloc(a[i])
        inc(i)
      dealloc(a)

    proc atomicInc*(memLoc: var int, x: int = 1): int {.inline, discardable.}
      ## atomic increment of `memLoc`. Returns the value after the operation.
    
    proc atomicDec*(memLoc: var int, x: int = 1): int {.inline, discardable.}
      ## atomic decrement of `memLoc`. Returns the value after the operation.

    include "system/atomics"


  proc cmp(x, y: string): int =
    result = int(c_strcmp(x, y))

  when not defined(NimrodVM):
    proc likely*(val: bool): bool {.importc: "likely", nodecl, nosideeffect.}
      ## can be used to mark a condition to be likely. This is a hint for the 
      ## optimizer.
    
    proc unlikely*(val: bool): bool {.importc: "unlikely", nodecl, nosideeffect.}
      ## can be used to mark a condition to be unlikely. This is a hint for the 
      ## optimizer.
      
    proc rawProc*[T: proc](x: T): pointer {.noSideEffect, inline.} =
      ## retrieves the raw proc pointer of the closure `x`. This is
      ## useful for interfacing closures with C.
      {.emit: """
      `result` = `x`.ClPrc;
      """.}

    proc rawEnv*[T: proc](x: T): pointer {.noSideEffect, inline.} =
      ## retrieves the raw environment pointer of the closure `x`. This is
      ## useful for interfacing closures with C.
      {.emit: """
      `result` = `x`.ClEnv;
      """.}

    proc finished*[T: proc](x: T): bool {.noSideEffect, inline.} =
      ## can be used to determine if a first class iterator has finished.
      {.emit: """
      `result` = *((NI*) `x`.ClEnv) < 0;
      """.}

elif defined(JS):
  # Stubs:
  proc nimGCvisit(d: pointer, op: int) {.compilerRtl.} = nil

  proc GC_disable() = nil
  proc GC_enable() = nil
  proc GC_fullCollect() = nil
  proc GC_setStrategy(strategy: TGC_Strategy) = nil
  proc GC_enableMarkAndSweep() = nil
  proc GC_disableMarkAndSweep() = nil
  proc GC_getStatistics(): string = return ""
  
  proc getOccupiedMem(): int = return -1
  proc getFreeMem(): int = return -1
  proc getTotalMem(): int = return -1

  proc dealloc(p: pointer) = nil
  proc alloc(size: int): pointer = nil
  proc alloc0(size: int): pointer = nil
  proc realloc(p: Pointer, newsize: int): pointer = nil

  proc allocShared(size: int): pointer = nil
  proc allocShared0(size: int): pointer = nil
  proc deallocShared(p: pointer) = nil
  proc reallocShared(p: pointer, newsize: int): pointer = nil

  include "system/jssys"
  include "system/reprjs"

  proc add*(x: var string, y: cstring) {.noStackFrame.} =
    asm """
      var len = `x`[0].length-1;
      for (var i = 0; i < `y`.length; ++i) {
        `x`[0][len] = `y`.charCodeAt(i);
        ++len;
      }
      `x`[0][len] = 0
    """
  {.push stack_trace: off, profiler:off.}
  include "system/excpt"
  {.pop.}

elif defined(NimrodVM):
  proc cmp(x, y: string): int =
    if x == y: return 0
    if x < y: return -1
    return 1
  
when defined(nimffi):
  include "system/sysio"

proc quit*(errormsg: string, errorcode = QuitFailure) {.noReturn.} =
  ## a shorthand for ``echo(errormsg); quit(errorcode)``.
  echo(errormsg)
  quit(errorcode)

{.pop.} # checks
{.pop.} # hints

proc `/`*(x, y: int): float {.inline, noSideEffect.} =
  ## integer division that results in a float.
  result = toFloat(x) / toFloat(y)

proc slurp*(filename: string): string {.magic: "Slurp".}
proc staticRead*(filename: string): string {.magic: "Slurp".}
  ## compile-time ``readFile`` proc for easy `resource`:idx: embedding:
  ##
  ## .. code-block:: nimrod
  ##     const myResource = staticRead"mydatafile.bin"
  ##
  ## ``slurp`` is an alias for ``staticRead``.

proc gorge*(command: string, input = ""): string {.
  magic: "StaticExec".} = nil
proc staticExec*(command: string, input = ""): string {.
  magic: "StaticExec".} = nil
  ## executes an external process at compile-time.
  ## if `input` is not an empty string, it will be passed as a standard input
  ## to the executed program.
  ##
  ## .. code-block:: nimrod
  ##     const buildInfo = "Revision " & staticExec("git rev-parse HEAD") & 
  ##                       "\nCompiled on " & staticExec("uname -v")
  ##
  ## ``gorge`` is an alias for ``staticExec``.

proc `+=`*[T: TOrdinal](x: var T, y: T) {.magic: "Inc", noSideEffect.}
  ## Increments an ordinal

proc `-=`*[T: TOrdinal](x: var T, y: T) {.magic: "Dec", noSideEffect.}
  ## Decrements an ordinal

proc `*=`*[T: TOrdinal](x: var T, y: T) {.inline, noSideEffect.} =
  ## Binary `*=` operator for ordinals
  x = x * y

proc `+=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Increments in placee a floating point number
  x = x + y

proc `-=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Decrements in place a floating point number
  x = x - y

proc `*=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Multiplies in place a floating point number
  x = x * y

proc `/=`*[T: float|float32|float64] (x: var T, y: T) {.inline, noSideEffect.} =
  ## Divides in place a floating point number
  x = x / y

proc `&=`* (x: var string, y: string) {.magic: "AppendStrStr", noSideEffect.}

proc rand*(max: int): int {.magic: "Rand", sideEffect.}
  ## compile-time `random` function. Useful for debugging.

proc astToStr*[T](x: T): string {.magic: "AstToStr", noSideEffect.}
  ## converts the AST of `x` into a string representation. This is very useful
  ## for debugging.
  
proc InstantiationInfo*(index = -1, fullPaths = false): tuple[
  filename: string, line: int] {. magic: "InstantiationInfo", noSideEffect.}
  ## provides access to the compiler's instantiation stack line information.
  ##
  ## This proc is mostly useful for meta programming (eg. ``assert`` template)
  ## to retrieve information about the current filename and line number.
  ## Example:
  ##
  ## .. code-block:: nimrod
  ##   import strutils
  ##
  ##   template testException(exception, code: expr): stmt =
  ##     try:
  ##       let pos = instantiationInfo()
  ##       discard(code)
  ##       echo "Test failure at $1:$2 with '$3'" % [pos.filename,
  ##         $pos.line, astToStr(code)]
  ##       assert false, "A test expecting failure succeeded?"
  ##     except exception:
  ##       nil
  ##
  ##   proc tester(pos: int): int =
  ##     let
  ##       a = @[1, 2, 3]
  ##     result = a[pos]
  ##
  ##   when isMainModule:
  ##     testException(EInvalidIndex, tester(30))
  ##     testException(EInvalidIndex, tester(1))
  ##     # --> Test failure at example.nim:20 with 'tester(1)'

template CurrentSourcePath*: string = InstantiationInfo(-1, true).filename
  ## returns the full file-system path of the current source

proc raiseAssert*(msg: string) {.noinline.} =
  raise newException(EAssertionFailed, msg)

when true:
  proc hiddenRaiseAssert(msg: string) {.raises: [], tags: [].} =
    # trick the compiler to not list ``EAssertionFailed`` when called
    # by ``assert``.
    type THide = proc (msg: string) {.noinline, raises: [], noSideEffect,
                                      tags: [].}
    THide(raiseAssert)(msg)

when not defined(nimhygiene):
  {.pragma: inject.}

proc shallow*(s: var string) {.noSideEffect, inline.} =
  ## marks a string `s` as `shallow`:idx:. Subsequent assignments will not
  ## perform deep copies of `s`. This is only useful for optimization 
  ## purposes.
  when not defined(JS) and not defined(NimrodVM):
    var s = cast[PGenericSeq](s)
    s.reserved = s.reserved or seqShallowFlag

type
  TNimrodNode {.final.} = object
  PNimrodNode* {.magic: "PNimrodNode".} = ref TNimrodNode
    ## represents a Nimrod AST node. Macros operate on this type.

template eval*(blk: stmt): stmt =
  ## executes a block of code at compile time just as if it was a macro
  ## optionally, the block can return an AST tree that will replace the 
  ## eval expression
  macro payload: stmt {.gensym.} = blk
  payload()

proc compiles*(x: expr): bool {.magic: "Compiles", noSideEffect.} =
  ## Special compile-time procedure that checks whether `x` can be compiled
  ## without any semantic error.
  ## This can be used to check whether a type supports some operation:
  ##
  ## .. code-block:: Nimrod
  ##   when not compiles(3 + 4):
  ##     echo "'+' for integers is available"
  nil

when defined(initDebugger):
  initDebugger()

proc locals*(): TObject {.magic: "Locals", noSideEffect.} =
  ## generates a tuple constructor expression listing all the local variables
  ## in the current scope. This is quite fast as it does not rely
  ## on any debug or runtime information. Note that in constrast to what
  ## the official signature says, the return type is not ``TObject`` but a
  ## tuple of a structure that depends on the current scope.
  nil

template assert*(cond: bool, msg = "") =
  ## provides a means to implement `programming by contracts`:idx: in Nimrod.
  ## ``assert`` evaluates expression ``cond`` and if ``cond`` is false, it
  ## raises an ``EAssertionFailure`` exception. However, the compiler may
  ## not generate any code at all for ``assert`` if it is advised to do so.
  ## Use ``assert`` for debugging purposes only.
  bind InstantiationInfo, hiddenRaiseAssert
  when compileOption("assertions"):
    {.line.}:
      if not cond:
        hiddenRaiseAssert(astToStr(cond) & ' ' & msg)

template doAssert*(cond: bool, msg = "") =
  ## same as `assert` but is always turned on and not affected by the
  ## ``--assertions`` command line switch.
  bind InstantiationInfo
  {.line: InstantiationInfo().}:
    if not cond:
      raiseAssert(astToStr(cond) & ' ' & msg)

template onFailedAssert*(msg: expr, code: stmt): stmt =
  ## Sets an assertion failure handler that will intercept any assert statements
  ## following `onFailedAssert` in the current lexical scope.
  ## Can be defined multiple times in a single function.
  ##  
  ## .. code-block:: nimrod
  ##
  ##   proc example(x: int): TErrorCode =
  ##     onFailedAssert(msg):
  ##       log msg
  ##       return E_FAIL
  ## 
  ##     assert(...)
  ##     
  ##     onFailedAssert(msg):
  ##       raise newException(EMyException, msg)
  ##
  ##     assert(...)
  ##
  template raiseAssert(msgIMPL: string): stmt =
    let msg {.inject.} = msgIMPL
    code

when not defined(noDynamicAlloc):
  include "sys/strings2"
  include "sys/exceptions2"
