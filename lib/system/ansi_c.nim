#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This include file contains headers of Ansi C procs
# and definitions of Ansi C types in Nimrod syntax
# All symbols are prefixed with 'c_' to avoid ambiguities

{.push hints:off.}

when hostOS != "standalone":
  include "system/ansi_c_os"

proc c_strcmp(a, b: CString): cint {.nodecl, noSideEffect, importc: "strcmp".}
proc c_memcmp(a, b: CString, size: int): cint {.
  nodecl, noSideEffect, importc: "memcmp".}
proc c_memcpy(a, b: CString, size: int) {.nodecl, importc: "memcpy".}
proc c_strlen(a: CString): int {.nodecl, noSideEffect, importc: "strlen".}
proc c_memset(p: pointer, value: cint, size: int) {.nodecl, importc: "memset".}

type
  C_JmpBuf {.importc: "jmp_buf".} = array[0..31, int]

proc c_longjmp(jmpb: C_JmpBuf, retval: cint) {.nodecl, importc: "longjmp".}
proc c_setjmp(jmpb: var C_JmpBuf): cint {.nodecl, importc: "setjmp".}

proc c_printf(frmt: CString) {.
  importc: "printf", nodecl, varargs.}

proc c_sprintf(buf, frmt: CString) {.nodecl, importc: "sprintf", varargs,
                                     noSideEffect.}
  # we use it only in a way that cannot lead to security issues

{.pop.}
