#
#
#      c2nim - C to Nimrod source converter
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import 
  strutils, os, times, parseopt, llstream, ast, rnimsyn, options, msgs,
  clex, cparse

const
  Version = "0.8.10"
  Usage = """
c2nim - C to Nimrod source converter
  (c) 2010 Andreas Rumpf
Usage: c2nim [options] inputfile [options]
Options: 
  -o, --out:FILE         set output filename
  --dynlib:SYMBOL        import from dynlib: SYMBOL will be used for the import
  --header:HEADER_FILE   import from a HEADER_FILE (discouraged!)
  --cdecl                annotate procs with ``{.cdecl.}`` 
  --stdcall              annotate procs with ``{.stdcall.}``
  --ref                  convert typ* to ref typ (default: ptr typ)
  --prefix:PREFIX        strip prefix for the generated Nimrod identifiers 
                         (multiple --prefix options are supported)
  --suffix:SUFFIX        strip suffix for the generated Nimrod identifiers 
                         (multiple --suffix options are supported)
  -v, --version          write c2nim's version
  -h, --help             show this help
"""

proc main(infile, outfile: string, options: PParserOptions) = 
  var start = getTime()
  var stream = LLStreamOpen(infile, fmRead)
  if stream == nil: rawMessage(errCannotOpenFile, infile)
  var p: TParser
  openParser(p, infile, stream, options)
  var module = parseUnit(p)
  closeParser(p)
  renderModule(module, outfile)
  rawMessage(hintSuccessX, [$gLinesCompiled, $(getTime() - start)])

var
  infile = ""
  outfile = ""
  parserOptions = newParserOptions()
for kind, key, val in getopt():
  case kind
  of cmdArgument: infile = key
  of cmdLongOption, cmdShortOption:
    case key.toLower
    of "help", "h":
      stdout.write(Usage)
      quit(0)
    of "version", "v":
      stdout.write(Version & "\n")
      quit(0)
    of "o", "out": outfile = key
    else:
      if not parserOptions.setOption(key, val):
        stdout.write("[Error] unknown option: " & key)
  of cmdEnd: assert(false)
if infile.len == 0:
  # no filename has been given, so we show the help:
  stdout.write(Usage)
else:
  if outfile.len == 0:
    outfile = changeFileExt(infile, "nim")
  infile = addFileExt(infile, "h")
  main(infile, outfile, parserOptions)
