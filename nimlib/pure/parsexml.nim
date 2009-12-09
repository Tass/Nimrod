#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2009 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple high performance `XML`:idx: / `HTML`:idx:
## parser. 
## The only encoding that is supported is UTF-8. The parser has been designed
## to be somewhat error correcting, so that even most "wild HTML" found on the 
## web can be parsed with it. **Note:** This parser does not check that each
## ``<tag>`` has a corresponding ``</tag>``! These checks have do be 
## implemented by the client code for various reasons: 
##
## * Old HTML contains tags that have no end tag: ``<br>`` for example.
## * HTML tags are case insensitive, XML tags are case sensitive. Since this
##   library can parse both, only the client knows which comparison is to be
##   used.
## * Thus the checks would have been very difficult to implement properly with
##   little benefit, especially since they are simple to implement in the 
##   client. The client should use the `errorMsgExpected` proc to generate
##   a nice error message that fits the other error messages this library
##   creates.
##
##
## Example 1: Retrieve HTML title
## ==============================
##
## The file ``examples/htmltitle.nim`` demonstrates how to use the 
## XML parser to accomplish a simple task: To determine the title of an HTML
## document.
##
## .. code-block:: nimrod
##     :file: examples/htmltitle.nim
##
##
## Example 2: Retrieve all HTML links
## ==================================
##
## The file ``examples/htmlrefs.nim`` demonstrates how to use the 
## XML parser to accomplish another simple task: To determine all the links 
## an HTML document contains.
##
## .. code-block:: nimrod
##     :file: examples/htmlrefs.nim
##

import 
  hashes, strutils, lexbase, streams, unicode

# the parser treats ``<br />`` as ``<br></br>``

type 
  TXmlEventKind* = enum ## enumation of all events that may occur when parsing
    xmlError,           ## an error ocurred during parsing
    xmlEof,             ## end of file reached
    xmlCharData,        ## character data
    xmlWhitespace,      ## whitespace has been parsed
    xmlComment,         ## a comment has been parsed
    xmlPI,              ## processing instruction (``<?name something ?>``)
    xmlElementStart,    ## ``<elem>``
    xmlElementEnd,      ## ``</elem>``
    xmlElementOpen,     ## ``<elem 
    xmlAttribute,       ## ``key = "value"`` pair
    xmlElementClose,    ## ``>`` 
    xmlCData,           ## ``<![CDATA[`` ... data ... ``]]>``
    xmlEntity,          ## &entity;
    xmlSpecial          ## ``<! ... data ... >``
    
  TXmlError* = enum          ## enumeration that lists all errors that can occur
    errNone,                 ## no error
    errEndOfCDataExpected,   ## ``]]>`` expected
    errNameExpected,         ## name expected
    errSemicolonExpected,    ## ``;`` expected
    errQmGtExpected,         ## ``?>`` expected
    errGtExpected,           ## ``>`` expected
    errEqExpected,           ## ``=`` expected
    errQuoteExpected,        ## ``"`` or ``'`` expected
    errEndOfCommentExpected  ## ``-->`` expected
    
  TParserState = enum 
    stateStart, stateNormal, stateAttr, stateEmptyElementTag, stateError

  TXmlParseOption* = enum  ## options for the XML parser
    reportWhitespace,      ## report whitespace
    reportComments         ## report comments

  TXmlParser* = object of TBaseLexer ## the parser object.
    a, b: string
    kind: TXmlEventKind
    err: TXmlError
    state: TParserState
    filename: string
    options: set[TXmlParseOption]
 
const
  errorMessages: array [TXmlError, string] = [
    "no error",
    "']]>' expected",
    "name expected",
    "';' expected",
    "'?>' expected",
    "'>' expected",
    "'=' expected",
    "'\"' or \"'\" expected",
    "'-->' expected"
  ]

proc open*(my: var TXmlParser, input: PStream, filename: string,
           options: set[TXmlParseOption] = {}) =
  ## initializes the parser with an input stream. `Filename` is only used
  ## for nice error messages. The parser's behaviour can be controlled by
  ## the `options` parameter: If `options` contains ``reportWhitespace``
  ## a whitespace token is reported as an ``xmlWhitespace`` event.
  ## If `options` contains ``reportComments`` a comment token is reported as an
  ## ``xmlComment`` event. 
  lexbase.open(my, input)
  my.filename = filename
  my.state = stateStart
  my.kind = xmlError
  my.a = ""
  my.b = ""
  my.options = options
  
proc close*(my: var TXmlParser) {.inline.} = 
  ## closes the parser `my` and its associated input stream.
  lexbase.close(my)

proc charData*(my: TXmlParser): string {.inline.} = 
  ## returns the character data for the events: ``xmlCharData``, 
  ## ``xmlWhitespace``, ``xmlComment``, ``xmlCData``, ``xmlSpecial``
  assert(my.kind in {xmlCharData, xmlWhitespace, xmlComment, xmlCData, 
                     xmlSpecial})
  return my.a

proc kind*(my: TXmlParser): TXmlEventKind {.inline.} = 
  ## returns the current event type for the XML parser
  return my.kind

proc elementName*(my: TXmlParser): string {.inline.} = 
  ## returns the element name for the events: ``xmlElementStart``, 
  ## ``xmlElementEnd``, ``xmlElementOpen``
  assert(my.kind in {xmlElementStart, xmlElementEnd, xmlElementOpen})
  return my.a

proc entityName*(my: TXmlParser): string {.inline.} = 
  ## returns the entity name for the event: ``xmlEntity``
  assert(my.kind == xmlEntity)
  return my.a
  
proc attrKey*(my: TXmlParser): string {.inline.} = 
  ## returns the attribute key for the event ``xmlAttribute``
  assert(my.kind == xmlAttribute)
  return my.a
  
proc attrValue*(my: TXmlParser): string {.inline.} = 
  ## returns the attribute value for the event ``xmlAttribute``
  assert(my.kind == xmlAttribute)
  return my.b

proc PIName*(my: TXmlParser): string {.inline.} = 
  ## returns the processing instruction name for the event ``xmlPI``
  assert(my.kind == xmlPI)
  return my.a

proc PIRest*(my: TXmlParser): string {.inline.} = 
  ## returns the rest of the processing instruction for the event ``xmlPI``
  assert(my.kind == xmlPI)
  return my.b

proc getColumn*(my: TXmlParser): int {.inline.} = 
  ## get the current column the parser has arrived at.
  result = getColNumber(my, my.bufPos)

proc getLine*(my: TXmlParser): int {.inline.} = 
  ## get the current line the parser has arrived at.
  result = my.linenumber

proc getFilename*(my: TXmlParser): string {.inline.} = 
  ## get the filename of the file that the parser processes.
  result = my.filename
  
proc errorMsg*(my: TXmlParser): string = 
  ## returns a helpful error message for the event ``xmlError``
  assert(my.kind == xmlError)
  result = "$1($2, $3) Error: $4" % [
    my.filename, $getLine(my), $getColumn(my), errorMessages[my.err]]

proc errorMsgExpected*(my: TXmlParser, tag: string): string = 
  ## returns an error message "<tag> expected" in the same format as the
  ## other error messages 
  result = "$1($2, $3) Error: $4" % [
    my.filename, $getLine(my), $getColumn(my), "<$1> expected" % tag]
    
proc markError(my: var TXmlParser, kind: TXmlError) {.inline.} = 
  my.err = kind
  my.state = stateError

proc parseCDATA(my: var TXMLParser) = 
  var pos = my.bufpos + len("<![CDATA[")
  var buf = my.buf
  while true:
    case buf[pos] 
    of ']':
      if buf[pos+1] == ']' and buf[pos+2] == '>':
        inc(pos, 3)
        break
      add(my.a, ']')
      inc(pos)
    of '\0': 
      markError(my, errEndOfCDataExpected)
      break
    of '\c': 
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf
      add(my.a, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      add(my.a, '\L')
    else:
      add(my.a, buf[pos])
      inc(pos)    
  my.bufpos = pos # store back
  my.kind = xmlCDATA

proc parseComment(my: var TXMLParser) = 
  var pos = my.bufpos + len("<!--")
  var buf = my.buf
  while true:
    case buf[pos] 
    of '-':
      if buf[pos+1] == '-' and buf[pos+2] == '>':
        inc(pos, 3)
        break
      if my.options.contains(reportComments): add(my.a, '-')
      inc(pos)
    of '\0': 
      markError(my, errEndOfCommentExpected)
      break
    of '\c': 
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf
      if my.options.contains(reportComments): add(my.a, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      if my.options.contains(reportComments): add(my.a, '\L')
    else:
      if my.options.contains(reportComments): add(my.a, buf[pos])
      inc(pos)
  my.bufpos = pos
  my.kind = xmlComment

proc parseWhitespace(my: var TXmlParser, skip=False) = 
  var pos = my.bufpos
  var buf = my.buf
  while true: 
    case buf[pos]
    of ' ', '\t': 
      if not skip: add(my.a, buf[pos])
      Inc(pos)
    of '\c':  
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf
      if not skip: add(my.a, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      if not skip: add(my.a, '\L')
    else:
      break
  my.bufpos = pos

const
  NameStartChar = {'A'..'Z', 'a'..'z', '_', ':', '\128'..'\255'}
  NameChar = {'A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_', ':', '\128'..'\255'}

proc parseName(my: var TXmlParser, dest: var string) = 
  var pos = my.bufpos
  var buf = my.buf
  if buf[pos] in nameStartChar: 
    while true:
      add(dest, buf[pos])
      inc(pos)
      if buf[pos] notin NameChar: break
    my.bufpos = pos
  else:
    markError(my, errNameExpected)

proc parseEntity(my: var TXmlParser, dest: var string) = 
  var pos = my.bufpos+1
  var buf = my.buf
  my.kind = xmlCharData
  if buf[pos] == '#':
    var r: int
    inc(pos)
    if buf[pos] == 'x': 
      inc(pos)
      while true:
        case buf[pos]
        of '0'..'9': r = (r shl 4) or (ord(buf[pos]) - ord('0'))
        of 'a'..'f': r = (r shl 4) or (ord(buf[pos]) - ord('a') + 10)
        of 'A'..'F': r = (r shl 4) or (ord(buf[pos]) - ord('A') + 10)
        else: break
        inc(pos)
    else:
      while buf[pos] in {'0'..'9'}: 
        r = r * 10 + (ord(buf[pos]) - ord('0'))
        inc(pos)
    add(dest, toUTF8(TRune(r)))
  elif buf[pos] == 'l' and buf[pos+1] == 't':
    add(dest, '<')
    inc(pos, 2)
  elif buf[pos] == 'g' and buf[pos+1] == 't':
    add(dest, '>')
    inc(pos, 2)
  elif buf[pos] == 'a' and buf[pos+1] == 'm' and buf[pos+2] == 'p':
    add(dest, '&')
    inc(pos, 3)
  elif buf[pos] == 'a' and buf[pos+1] == 'p' and buf[pos+2] == 'o' and 
      buf[pos+3] == 's':
    add(dest, '\'')
    inc(pos, 4)
  elif buf[pos] == 'q' and buf[pos+1] == 'u' and buf[pos+2] == 'o' and 
      buf[pos+3] == 't':
    add(dest, '"')
    inc(pos, 4)
  else:
    my.bufpos = pos
    parseName(my, dest)
    pos = my.bufpos
    if my.err != errNameExpected: 
      my.kind = xmlEntity
    else:
      add(dest, '&')
  if buf[pos] == ';': 
    inc(pos)
  else:
    markError(my, errSemiColonExpected)
  my.bufpos = pos

proc parsePI(my: var TXmlParser) = 
  inc(my.bufpos, "<?".len)
  parseName(my, my.a)
  var pos = my.bufpos
  var buf = my.buf
  setLen(my.b, 0)
  while true: 
    case buf[pos]
    of '\0':
      markError(my, errQmGtExpected)
      break
    of '?':
      if buf[pos+1] == '>':
        inc(pos, 2)
        break
      add(my.b, '?')
      inc(pos)
    of '\c':  
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf      
      add(my.b, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      add(my.b, '\L')
    else:
      add(my.b, buf[pos])
      inc(pos)
  my.bufpos = pos
  my.kind = xmlPI

proc parseSpecial(my: var TXmlParser) = 
  # things that start with <!
  var pos = my.bufpos + 2
  var buf = my.buf
  var opentags = 0
  while true: 
    case buf[pos]
    of '\0':
      markError(my, errGtExpected)
      break
    of '<': 
      inc(opentags)
      inc(pos)
      add(my.a, '<')
    of '>':
      if opentags <= 0:
        inc(pos)
        break
      dec(opentags)
      inc(pos)
      add(my.a, '>')
    of '\c':  
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf
      add(my.a, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      add(my.a, '\L')
    else:
      add(my.a, buf[pos])
      inc(pos)
  my.bufpos = pos
  my.kind = xmlSpecial

proc parseTag(my: var TXmlParser) = 
  inc(my.bufpos)
  parseName(my, my.a)
  # if we have no name, do not interpret the '<':
  if my.a.len == 0: 
    my.kind = xmlCharData
    add(my.a, '<')
    return
  parseWhitespace(my, skip=True)
  if my.buf[my.bufpos] in NameStartChar: 
    # an attribute follows:
    my.kind = xmlElementOpen
    my.state = stateAttr
  else:
    my.kind = xmlElementStart
    if my.buf[my.bufpos] == '/' and my.buf[my.bufpos+1] == '>':
      inc(my.bufpos, 2)
      my.state = stateEmptyElementTag
    elif my.buf[my.bufpos] == '>':
      inc(my.bufpos)  
    else:
      markError(my, errGtExpected)
  
proc parseEndTag(my: var TXmlParser) = 
  inc(my.bufpos, 2)
  parseName(my, my.a)
  parseWhitespace(my, skip=True)
  if my.buf[my.bufpos] == '>':
    inc(my.bufpos)
  else:
    markError(my, errGtExpected)
  my.kind = xmlElementEnd

proc parseAttribute(my: var TXmlParser) = 
  my.kind = xmlAttribute
  setLen(my.a, 0)
  setLen(my.b, 0)
  parseName(my, my.a)
  # if we have no name, we have '<tag attr= key %&$$%':
  if my.a.len == 0: 
    markError(my, errGtExpected)
    return
  parseWhitespace(my, skip=True)
  if my.buf[my.bufpos] != '=':
    markError(my, errEqExpected)
    return
  inc(my.bufpos)
  parseWhitespace(my, skip=True)

  var pos = my.bufpos
  var buf = my.buf
  if buf[pos] in {'\'', '"'}:
    var quote = buf[pos]
    var pendingSpace = false
    inc(pos)
    while true: 
      case buf[pos]
      of '\0':
        markError(my, errQuoteExpected)
        break
      of '&': 
        if pendingSpace: 
          add(my.b, ' ')
          pendingSpace = false
        my.bufpos = pos
        parseEntity(my, my.b)
        my.kind = xmlAttribute # parseEntity overwrites my.kind!
        pos = my.bufpos
      of ' ', '\t': 
        pendingSpace = true
        inc(pos)
      of '\c':  
        pos = lexbase.HandleCR(my, pos)
        buf = my.buf
        pendingSpace = true
      of '\L': 
        pos = lexbase.HandleLF(my, pos)
        buf = my.buf
        pendingSpace = true
      else:
        if buf[pos] == quote:
          inc(pos)
          break
        else:
          if pendingSpace: 
            add(my.b, ' ')
            pendingSpace = false
          add(my.b, buf[pos])
          inc(pos)
  else:
    markError(my, errQuoteExpected)  
  my.bufpos = pos
  parseWhitespace(my, skip=True)
  
proc parseCharData(my: var TXmlParser) = 
  var pos = my.bufpos
  var buf = my.buf
  while true: 
    case buf[pos]
    of '\0', '<', '&': break
    of '\c':  
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.HandleCR(my, pos)
      buf = my.buf
      add(my.a, '\L')
    of '\L': 
      pos = lexbase.HandleLF(my, pos)
      buf = my.buf
      add(my.a, '\L')
    else:
      add(my.a, buf[pos])
      inc(pos)
  my.bufpos = pos
  my.kind = xmlCharData

proc rawGetTok(my: var TXmlParser) = 
  my.kind = xmlError
  setLen(my.a, 0)
  var pos = my.bufpos
  var buf = my.buf
  case buf[pos]
  of '<': 
    case buf[pos+1]
    of '/':
      parseEndTag(my)
    of '!':
      if buf[pos+2] == '[' and buf[pos+3] == 'C' and buf[pos+4] == 'D' and
          buf[pos+5] == 'A' and buf[pos+6] == 'T' and buf[pos+7] == 'A' and
          buf[pos+8] == '[':
        parseCDATA(my)
      elif buf[pos+2] == '-' and buf[pos+3] == '-': 
        parseComment(my)
      else: 
        parseSpecial(my)
    of '?':
      parsePI(my)
    else: 
      parseTag(my)
  of ' ', '\t', '\c', '\l': 
    parseWhiteSpace(my)
    my.kind = xmlWhitespace
  of '\0': 
    my.kind = xmlEof
  of '&':
    parseEntity(my, my.a)
  else: 
    parseCharData(my)
  assert my.kind != xmlError
    
proc getTok(my: var TXmlParser) = 
  while true:
    rawGetTok(my)
    case my.kind
    of xmlComment: 
      if my.options.contains(reportComments): break
    of xmlWhitespace: 
      if my.options.contains(reportWhitespace): break
    else: break
    
proc next*(my: var TXmlParser) = 
  ## retrieves the first/next event. This controls the parser.
  case my.state
  of stateNormal:
    getTok(my)  
  of stateStart:
    getTok(my)
    if my.kind == xmlPI and my.a == "xml": 
      # just skip the first ``<?xml >`` processing instruction
      getTok(my)
    my.state = stateNormal
  of stateAttr:
    # parse an attribute key-value pair:
    if my.buf[my.bufpos] == '>':
      my.kind = xmlElementClose
      inc(my.bufpos)
      my.state = stateNormal
    elif my.buf[my.bufpos] == '/' and my.buf[my.bufpos+1] == '>': 
      my.kind = xmlElementClose
      inc(my.bufpos, 2)
      my.state = stateEmptyElementTag
    else:
      parseAttribute(my)
      # state remains the same
  of stateEmptyElementTag:
    my.state = stateNormal
    my.kind = xmlElementEnd
  of stateError: 
    my.kind = xmlError
    my.state = stateNormal
  
when isMainModule:
  import os
  var s = newFileStream(ParamStr(1), fmRead)
  if s == nil: quit("cannot open the file" & ParamStr(1))
  var x: TXmlParser
  open(x, s, ParamStr(1))
  while true:
    next(x)
    case x.kind
    of xmlError: Echo(x.errorMsg())
    of xmlEof: break
    of xmlCharData: echo(x.charData)
    of xmlWhitespace: echo("|$1|" % x.charData)
    of xmlComment: echo("<!-- $1 -->" % x.charData)
    of xmlPI: echo("<? $1 ## $2 ?>" % [x.PIName, x.PIRest])
    of xmlElementStart: echo("<$1>" % x.elementName)
    of xmlElementEnd: echo("</$1>" % x.elementName)
    
    of xmlElementOpen: echo("<$1" % x.elementName) 
    of xmlAttribute:   
      echo("Key: " & x.attrKey)
      echo("Value: " & x.attrValue)
    
    of xmlElementClose: echo(">") 
    of xmlCData:
      echo("<![CDATA[$1]]>" % x.charData)
    of xmlEntity:
      echo("&$1;" % x.entityName)
    of xmlSpecial:
      echo("SPECIAL: " & x.charData)
  close(x)

