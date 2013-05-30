# undocumented:
proc getRefcount*[T](x: ref T): int {.importc: "getRefcount", noSideEffect.}
proc getRefcount*(x: string): int {.importc: "getRefcount", noSideEffect.}
proc getRefcount*[T](x: seq[T]): int {.importc: "getRefcount", noSideEffect.}
  ## retrieves the reference count of an heap-allocated object. The
  ## value is implementation-dependent.

# GC interface:
when not defined(nimrodVM):
  proc getOccupiedMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process and hold data.

  proc getFreeMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process, but do not
    ## hold any meaningful data.

  proc getTotalMem*(): int {.rtl.}
    ## returns the number of bytes that are owned by the process.

# ----------------- GC interface ---------------------------------------------

proc GC_ref*[T](x: ref T) {.magic: "GCref".}
proc GC_ref*[T](x: seq[T]) {.magic: "GCref".}
proc GC_ref*(x: string) {.magic: "GCref".}
  ## marks the object `x` as referenced, so that it will not be freed until
  ## it is unmarked via `GC_unref`. If called n-times for the same object `x`,
  ## n calls to `GC_unref` are needed to unmark `x`. 
  
proc GC_unref*[T](x: ref T) {.magic: "GCunref".}
proc GC_unref*[T](x: seq[T]) {.magic: "GCunref".}
proc GC_unref*(x: string) {.magic: "GCunref".}
  ## see the documentation of `GC_ref`.
