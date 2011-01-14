#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple HTTP-Server.
##
## Example:
##
## .. code-block:: nimrod
##  import strutils, sockets, httpserver
##
##  var counter = 0
##  proc handleRequest(client: TSocket, path, query: string): bool {.procvar.} =
##    inc(counter)
##    client.send("Hello for the $#th time." % $counter & wwwNL)
##    return false # do not stop processing
##
##  run(handleRequest, TPort(80))
##

import parseutils, strutils, os, osproc, strtabs, streams, sockets

const
  wwwNL* = "\r\L"
  ServerSig = "Server: httpserver.nim/1.0.0" & wwwNL

# --------------- output messages --------------------------------------------

proc sendTextContentType(client: TSocket) =
  send(client, "Content-type: text/html" & wwwNL)
  send(client, wwwNL)

proc badRequest(client: TSocket) =
  # Inform the client that a request it has made has a problem.
  send(client, "HTTP/1.0 400 BAD REQUEST" & wwwNL)
  sendTextContentType(client)
  send(client, "<p>Your browser sent a bad request, " &
               "such as a POST without a Content-Length." & wwwNL)

proc cannotExec(client: TSocket) =
  send(client, "HTTP/1.0 500 Internal Server Error" & wwwNL)
  sendTextContentType(client)
  send(client, "<P>Error prohibited CGI execution." & wwwNL)

proc headers(client: TSocket, filename: string) =
  # XXX could use filename to determine file type
  send(client, "HTTP/1.0 200 OK" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)

proc notFound(client: TSocket) =
  send(client, "HTTP/1.0 404 NOT FOUND" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)
  send(client, "<html><title>Not Found</title>" & wwwNL)
  send(client, "<body><p>The server could not fulfill" & wwwNL)
  send(client, "your request because the resource specified" & wwwNL)
  send(client, "is unavailable or nonexistent.</p>" & wwwNL)
  send(client, "</body></html>" & wwwNL)

proc unimplemented(client: TSocket) =
  send(client, "HTTP/1.0 501 Method Not Implemented" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)
  send(client, "<html><head><title>Method Not Implemented" &
               "</title></head>" &
               "<body><p>HTTP request method not supported.</p>" &
               "</body></HTML>" & wwwNL)

# ----------------- file serving ---------------------------------------------

proc discardHeaders(client: TSocket) = skip(client)

proc serveFile*(client: TSocket, filename: string) =
  ## serves a file to the client.
  when false: discardHeaders(client)
  var f: TFile
  if open(f, filename):
    headers(client, filename)
    const bufSize = 8000 # != 8K might be good for memory manager
    var buf = alloc(bufsize)
    while True:
      var bytesread = readBuffer(f, buf, bufsize)
      if bytesread > 0:
        var byteswritten = send(client, buf, bytesread)
        if bytesread != bytesWritten:
          dealloc(buf)
          close(f)
          OSError()
      if bytesread != bufSize: break
    dealloc(buf)
    close(f)
  else:
    notFound(client)

# ------------------ CGI execution -------------------------------------------

type
  TRequestMethod = enum reqGet, reqPost

proc executeCgi(client: TSocket, path, query: string, meth: TRequestMethod) =
  var env = newStringTable(modeCaseInsensitive)
  var contentLength = -1
  case meth
  of reqGet:
    discardHeaders(client)

    env["REQUEST_METHOD"] = "GET"
    env["QUERY_STRING"] = query
  of reqPost:
    var buf = ""
    var dataAvail = false
    while dataAvail:
      dataAvail = recvLine(client, buf)
      var L = toLower(buf)
      if L.startsWith("content-length:"):
        var i = len("content-length:")
        while L[i] in Whitespace: inc(i)
        contentLength = parseInt(copy(L, i))

    if contentLength < 0:
      badRequest(client)
      return

    env["REQUEST_METHOD"] = "POST"
    env["CONTENT_LENGTH"] = $contentLength

  send(client, "HTTP/1.0 200 OK" & wwwNL)

  var process = startProcess(command=path, env=env)
  if meth == reqPost:
    # get from client and post to CGI program:
    var buf = alloc(contentLength)
    if recv(client, buf, contentLength) != contentLength: 
      dealloc(buf)
      OSError()
    var inp = process.inputStream
    inp.writeData(inp, buf, contentLength)
    dealloc(buf)

  var outp = process.outputStream
  while running(process) or not outp.atEnd(outp):
    var line = outp.readLine()
    send(client, line)
    send(client, wwwNL)

# --------------- Server Setup -----------------------------------------------

proc acceptRequest(client: TSocket) =
  var cgi = false
  var query = ""
  var buf = ""
  discard recvLine(client, buf)
  var path = ""
  var data = buf.split()
  var meth = reqGet

  var q = find(data[1], '?')

  # extract path
  if q >= 0:
    # strip "?..." from path, this may be found in both POST and GET
    path = "." & data[1].copy(0, q-1)
  else:
    path = "." & data[1]
  # path starts with "/", by adding "." in front of it we serve files from cwd
  
  if cmpIgnoreCase(data[0], "GET") == 0:
    if q >= 0:
      cgi = true
      query = data[1].copy(q+1)
  elif cmpIgnoreCase(data[0], "POST") == 0:
    cgi = true
    meth = reqPost
  else:
    unimplemented(client)

  if path[path.len-1] == '/' or existsDir(path):
    path = path / "index.html"

  if not ExistsFile(path):
    discardHeaders(client)
    notFound(client)
  else:
    when defined(Windows):
      var ext = splitFile(path).ext.toLower
      if ext == ".exe" or ext == ".cgi":
        # XXX: extract interpreter information here?
        cgi = true
    else:
      if {fpUserExec, fpGroupExec, fpOthersExec} * path.getFilePermissions != {}:
        cgi = true
    if not cgi:
      serveFile(client, path)
    else:
      executeCgi(client, path, query, meth)

type
  TServer* = object       ## contains the current server state
    socket: TSocket
    port: TPort
    client*: TSocket      ## the socket to write the file data to
    path*, query*: string ## path and query the client requested

proc open*(s: var TServer, port = TPort(80)) =
  ## creates a new server at port `port`. If ``port == 0`` a free port is
  ## acquired that can be accessed later by the ``port`` proc.
  s.socket = socket(AF_INET)
  if s.socket == InvalidSocket: OSError()
  bindAddr(s.socket, port)
  listen(s.socket)

  if port == TPort(0):
    s.port = getSockName(s.socket)
  else:
    s.port = port
  s.client = InvalidSocket
  s.path = ""
  s.query = ""

proc port*(s: var TServer): TPort =
  ## get the port number the server has acquired.
  result = s.port

proc next*(s: var TServer) =
  ## proceed to the first/next request.
  s.client = accept(s.socket)
  headers(s.client, "")
  var data = recv(s.client)
  #discard recvLine(s.client, data)
  
  var i = skipWhitespace(data)
  if skipIgnoreCase(data, "GET") > 0: inc(i, 3)
  elif skipIgnoreCase(data, "POST") > 0: inc(i, 4)
  else: 
    unimplemented(s.client)
    return
  
  var L = skipWhitespace(data, i)
  inc(i, L)
  # XXX we ignore "HTTP/1.1" etc. for now here
  var query = 0
  var last = i
  while last < data.len and data[last] notin whitespace: 
    if data[last] == '?' and query == 0: query = last
    inc(last)
  if query > 0:
    s.query = data.copy(query+1, last-1)
    s.path = data.copy(i, query-1)
  else:
    s.query = ""
    s.path = data.copy(i, last-1)

proc close*(s: TServer) =
  ## closes the server (and the socket the server uses).
  close(s.socket)

proc run*(handleRequest: proc (client: TSocket, path, query: string): bool,
          port = TPort(80)) =
  ## encapsulates the server object and main loop
  var s: TServer
  open(s, port)
  #echo("httpserver running on port ", s.port)
  while true:
    next(s)
    if handleRequest(s.client, s.path, s.query): break
    close(s.client)
  close(s)

when isMainModule:
  var counter = 0

  var s: TServer
  open(s, TPort(0))
  echo("httpserver running on port ", s.port)
  while true:
    next(s)
    
    inc(counter)
    s.client.send("Hello, Andreas, for the $#th time. $# ? $#" % [
      $counter, s.path, s.query] & wwwNL)
    
    close(s.client)
  close(s)

