# Test for overloading

type
  TNone {.exportc: "_NONE", final.} = object

proc TNone(a, b: int) = nil #ERROR_MSG attempt to redefine 'TNone'
