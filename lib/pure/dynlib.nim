#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2009 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the ability to access symbols from shared
## libraries. On POSIX this uses the ``dlsym`` mechanism, on 
## Windows ``LoadLibrary``. 

type
  TLibHandle* = pointer ## a handle to a dynamically loaded library

proc LoadLib*(path: string): TLibHandle
  ## loads a library from `path`. Returns nil if the library could not 
  ## be loaded.

proc UnloadLib*(lib: TLibHandle)
  ## unloads the library `lib`

proc symAddr*(lib: TLibHandle, name: string): pointer
  ## retrieves the address of a procedure/variable from `lib`. Returns nil
  ## if the symbol could not be found.

proc checkedSymAddr*(lib: TLibHandle, name: string): pointer =
  ## retrieves the address of a procedure/variable from `lib`. Raises
  ## `EInvalidLibrary` if the symbol could not be found.
  result = symAddr(lib, name)
  if result == nil: 
    var e: ref EInvalidLibrary
    new(e)
    e.msg = "could not find symbol: " & name
    raise e

when defined(posix):
  #
  # =========================================================================
  # This is an implementation based on the dlfcn interface.
  # The dlfcn interface is available in Linux, SunOS, Solaris, IRIX, FreeBSD,
  # NetBSD, AIX 4.2, HPUX 11, and probably most other Unix flavors, at least
  # as an emulation layer on top of native functions.
  # =========================================================================
  #
  var
    RTLD_NOW {.importc: "RTLD_NOW", header: "<dlfcn.h>".}: int

  proc dlclose(lib: TLibHandle) {.importc, header: "<dlfcn.h>".}
  proc dlopen(path: CString, mode: int): TLibHandle {.
      importc, header: "<dlfcn.h>".}
  proc dlsym(lib: TLibHandle, name: cstring): pointer {.
      importc, header: "<dlfcn.h>".}

  proc LoadLib(path: string): TLibHandle = return dlopen(path, RTLD_NOW)
  proc UnloadLib(lib: TLibHandle) = dlclose(lib)
  proc symAddr(lib: TLibHandle, name: string): pointer = 
    return dlsym(lib, name)

elif defined(windows) or defined(dos):
  #
  # =======================================================================
  # Native Windows Implementation
  # =======================================================================
  #
  type
    THINSTANCE {.importc: "HINSTANCE".} = pointer

  proc FreeLibrary(lib: THINSTANCE) {.importc, header: "<windows.h>", stdcall.}
  proc winLoadLibrary(path: cstring): THINSTANCE {.
      importc: "LoadLibraryA", header: "<windows.h>", stdcall.}
  proc GetProcAddress(lib: THINSTANCE, name: cstring): pointer {.
      importc: "GetProcAddress", header: "<windows.h>", stdcall.}

  proc LoadLib(path: string): TLibHandle =
    result = cast[TLibHandle](winLoadLibrary(path))
  proc UnloadLib(lib: TLibHandle) = FreeLibrary(cast[THINSTANCE](lib))

  proc symAddr(lib: TLibHandle, name: string): pointer =
    result = GetProcAddress(cast[THINSTANCE](lib), name)

else:
  {.error: "no implementation for dynlib".}
