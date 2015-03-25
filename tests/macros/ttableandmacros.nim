import macros, tables, hashes

proc hash(v: NimNode): THash = 4  # performance is for suckers
macro test(body: stmt): stmt {.immediate.} =
  var a = initCountTable[NimNode]()
  a.inc(body)
  echo(a)

test:
  1 + 1
