# Test various aspects

import
  mvarious, io

type
  PA = ref TA
  PB = ref TB

  TB = record
    a: PA

  TA = record
    b: TB
    x: int

proc getPA(): PA =
  var
    b: bool
  b = not false
  return nil

var
  global: int

var
  s: string
  i: int
  r: TA

r.b.a.x = 0
global = global + 1
exportme()
write(stdout, "Hallo wie hei�t du? ")
write(stdout, getPA().x)
s = readLine(stdin)
i = 0
while i < s.len:
  if s[i] == 'c': write(stdout, "'c' in deinem Namen gefunden\n")
  i = i + 1

write(stdout, "Du hei�t " & s)

