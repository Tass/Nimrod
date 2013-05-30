proc new*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.

proc new*(T: typedesc): ref T =
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it as result value
  new(result)

proc unsafeNew*[T](a: var ref T, size: int) {.magic: "New", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``. This is **unsafe** as it allocates an object
  ## of the passed ``size``. This should only be used for optimization
  ## purposes when you know what you're doing!

proc internalNew*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## leaked implementation detail. Do not use.

proc new*[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``. When the garbage collector frees the object,
  ## `finalizer` is called. The `finalizer` may not keep a reference to the
  ## object pointed to by `x`. The `finalizer` cannot prevent the GC from
  ## freeing the object. Note: The `finalizer` refers to the type `T`, not to
  ## the object! This means that for each object of type `T` the finalizer
  ## will be called!
  
proc reset*[T](obj: var T) {.magic: "Reset", noSideEffect.}
  ## resets an object `obj` to its initial (binary zero) value. This needs to
  ## be called before any possible `object branch transition`:idx:.

type
  PObject* = ref TObject ## reference to TObject

proc `==` *[T](x, y: ref T): bool {.magic: "EqRef", noSideEffect.}
proc `<=` *[T](x, y: ref T): bool {.magic: "LePtr", noSideEffect.}
proc `<` *[T](x, y: ref T): bool {.magic: "LtPtr", noSideEffect.}
