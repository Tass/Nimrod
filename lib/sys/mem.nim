proc zeroMem*(p: Pointer, size: int) {.importc, noDecl.}
  ## overwrites the contents of the memory at ``p`` with the value 0.
  ## Exactly ``size`` bytes will be overwritten. Like any procedure
  ## dealing with raw memory this is *unsafe*.

proc copyMem*(dest, source: Pointer, size: int) {.importc: "memcpy", noDecl.}
  ## copies the contents from the memory at ``source`` to the memory
  ## at ``dest``. Exactly ``size`` bytes will be copied. The memory
  ## regions may not overlap. Like any procedure dealing with raw
  ## memory this is *unsafe*.

proc moveMem*(dest, source: Pointer, size: int) {.importc: "memmove", noDecl.}
  ## copies the contents from the memory at ``source`` to the memory
  ## at ``dest``. Exactly ``size`` bytes will be copied. The memory
  ## regions may overlap, ``moveMem`` handles this case appropriately
  ## and is thus somewhat more safe than ``copyMem``. Like any procedure
  ## dealing with raw memory this is still *unsafe*, though.

proc equalMem*(a, b: Pointer, size: int): bool {.
  importc: "equalMem", noDecl, noSideEffect.}
  ## compares the memory blocks ``a`` and ``b``. ``size`` bytes will
  ## be compared. If the blocks are equal, true is returned, false
  ## otherwise. Like any procedure dealing with raw memory this is
  ## *unsafe*.

proc alloc*(size: int): pointer {.noconv, rtl, tags: [].}
  ## allocates a new memory block with at least ``size`` bytes. The
  ## block has to be freed with ``realloc(block, 0)`` or
  ## ``dealloc(block)``. The block is not initialized, so reading
  ## from it before writing to it is undefined behaviour!
  ## The allocated memory belongs to its allocating thread!
  ## Use `allocShared` to allocate from a shared heap.
proc alloc0*(size: int): pointer {.noconv, rtl, tags: [].}
  ## allocates a new memory block with at least ``size`` bytes. The
  ## block has to be freed with ``realloc(block, 0)`` or
  ## ``dealloc(block)``. The block is initialized with all bytes
  ## containing zero, so it is somewhat safer than ``alloc``.
  ## The allocated memory belongs to its allocating thread!
  ## Use `allocShared0` to allocate from a shared heap.
proc realloc*(p: Pointer, newsize: int): pointer {.noconv, rtl, tags: [].}
  ## grows or shrinks a given memory block. If p is **nil** then a new
  ## memory block is returned. In either way the block has at least
  ## ``newsize`` bytes. If ``newsize == 0`` and p is not **nil**
  ## ``realloc`` calls ``dealloc(p)``. In other cases the block has to
  ## be freed with ``dealloc``.
  ## The allocated memory belongs to its allocating thread!
  ## Use `reallocShared` to reallocate from a shared heap.
proc dealloc*(p: Pointer) {.noconv, rtl, tags: [].}
  ## frees the memory allocated with ``alloc``, ``alloc0`` or
  ## ``realloc``. This procedure is dangerous! If one forgets to
  ## free the memory a leak occurs; if one tries to access freed
  ## memory (or just freeing it twice!) a core dump may happen
  ## or other memory may be corrupted. 
  ## The freed memory must belong to its allocating thread!
  ## Use `deallocShared` to deallocate from a shared heap.

proc allocShared*(size: int): pointer {.noconv, rtl.}
  ## allocates a new memory block on the shared heap with at
  ## least ``size`` bytes. The block has to be freed with
  ## ``reallocShared(block, 0)`` or ``deallocShared(block)``. The block
  ## is not initialized, so reading from it before writing to it is 
  ## undefined behaviour!
proc allocShared0*(size: int): pointer {.noconv, rtl.}
  ## allocates a new memory block on the shared heap with at 
  ## least ``size`` bytes. The block has to be freed with
  ## ``reallocShared(block, 0)`` or ``deallocShared(block)``.
  ## The block is initialized with all bytes
  ## containing zero, so it is somewhat safer than ``allocShared``.
proc reallocShared*(p: Pointer, newsize: int): pointer {.noconv, rtl.}
  ## grows or shrinks a given memory block on the heap. If p is **nil**
  ## then a new memory block is returned. In either way the block has at least
  ## ``newsize`` bytes. If ``newsize == 0`` and p is not **nil**
  ## ``reallocShared`` calls ``deallocShared(p)``. In other cases the
  ## block has to be freed with ``deallocShared``.
proc deallocShared*(p: Pointer) {.noconv, rtl.}
  ## frees the memory allocated with ``allocShared``, ``allocShared0`` or
  ## ``reallocShared``. This procedure is dangerous! If one forgets to
  ## free the memory a leak occurs; if one tries to access freed
  ## memory (or just freeing it twice!) a core dump may happen
  ## or other memory may be corrupted.

proc GC_disable*() {.rtl, inl.}
  ## disables the GC. If called n-times, n calls to `GC_enable` are needed to
  ## reactivate the GC. Note that in most circumstances one should only disable
  ## the mark and sweep phase with `GC_disableMarkAndSweep`.

proc GC_enable*() {.rtl, inl.}
  ## enables the GC again.

proc GC_fullCollect*() {.rtl.}
  ## forces a full garbage collection pass.
  ## Ordinary code does not need to call this (and should not).

type
  TGC_Strategy* = enum ## the strategy the GC should use for the application
    gcThroughput,      ## optimize for throughput
    gcResponsiveness,  ## optimize for responsiveness (default)
    gcOptimizeTime,    ## optimize for speed
    gcOptimizeSpace    ## optimize for memory footprint

proc GC_setStrategy*(strategy: TGC_Strategy) {.rtl, deprecated.}
  ## tells the GC the desired strategy for the application.
  ## **Deprecated** since version 0.8.14. This has always been a nop.

proc GC_enableMarkAndSweep*() {.rtl.}
proc GC_disableMarkAndSweep*() {.rtl.}
  ## the current implementation uses a reference counting garbage collector
  ## with a seldomly run mark and sweep phase to free cycles. The mark and
  ## sweep phase may take a long time and is not needed if the application
  ## does not create cycles. Thus the mark and sweep phase can be deactivated
  ## and activated separately from the rest of the GC.

proc GC_getStatistics*(): string {.rtl.}
  ## returns an informative string about the GC's activity. This may be useful
  ## for tweaking.
