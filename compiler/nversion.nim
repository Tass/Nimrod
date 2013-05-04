#
#
#           The Nimrod Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module contains Nimrod's version. It is the only place where it needs
# to be changed.

const 
  MaxSetElements* = 1 shl 16  # (2^16) to support unicode character sets?
  defaultAsmMarkerSymbol* = '!'
  VersionMajor* = 0
  VersionMinor* = 9
  VersionPatch* = 1
  VersionAsString* = $VersionMajor & "." & $VersionMinor & "." & $VersionPatch

  RodFileVersion* = "1212"       # modify this if the rod-format changes!

