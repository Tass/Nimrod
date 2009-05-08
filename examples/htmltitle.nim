# Example program to show the new parsexml module
# This program reads an HTML file and writes its title to stdout.
# Errors and whitespace are ignored.

import os, streams, parsexml, strutils

if paramCount() < 1: 
  quit("Usage: htmltitle filename[.html]")

var filename = appendFileExt(ParamStr(1), "html")
var s = newFileStream(filename, fmRead)
if s == nil: quit("cannot open the file " & filename)
var x: TXmlParser
open(x, s, filename)
while true:
  x.next()
  case x.kind
  of xmlElementStart: 
    if cmpIgnoreCase(x.elementName, "title") == 0: 
      var title = ""
      x.next()  # skip "<title>"
      while x.kind == xmlCharData: 
        title.add(x.charData)
        x.next()
      if x.kind == xmlElementEnd and cmpIgnoreCase(x.elementName, "title") == 0:
        Echo("Title: " & title)
        quit(0) # Success!
      else:
        echo(x.errorMsgExpected("/title"))
  
  of xmlEof: break # end of file reached
  else: nil # ignore other events

quit("Could not determine title!")
x.close()

