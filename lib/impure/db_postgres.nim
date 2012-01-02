#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A higher level `PostgreSQL`:idx: database wrapper. This interface 
## is implemented for other databases too.

import strutils, postgres

type
  TDbConn* = PPGconn   ## encapsulates a database connection
  TRow* = seq[string]  ## a row of a dataset
  EDb* = object of EIO ## exception that is raised if a database error occurs
  
  TSqlQuery* = distinct string ## an SQL query string
  
proc sql*(query: string): TSqlQuery {.noSideEffect, inline.} =  
  ## constructs a TSqlQuery from the string `query`. This is supposed to be 
  ## used as a raw-string-literal modifier:
  ## ``sql"update user set counter = counter + 1"``
  ##
  ## If assertions are turned off, it does nothing. If assertions are turned 
  ## on, later versions will check the string for valid syntax.
  result = TSqlQuery(query)
 
proc dbError(db: TDbConn) {.noreturn.} = 
  ## raises an EDb exception.
  var e: ref EDb
  new(e)
  e.msg = $PQerrorMessage(db)
  raise e

proc dbError*(msg: string) {.noreturn.} = 
  ## raises an EDb exception with message `msg`.
  var e: ref EDb
  new(e)
  e.msg = msg
  raise e

proc dbQuote(s: string): string =
  result = "'"
  for c in items(s):
    if c == '\'': add(result, "''")
    else: add(result, c)
  add(result, '\'')

proc dbFormat(formatstr: TSqlQuery, args: openarray[string]): string =
  result = ""
  var a = 0
  for c in items(string(formatstr)):
    if c == '?':
      add(result, dbQuote(args[a]))
      inc(a)
    else: 
      add(result, c)
  
proc TryExec*(db: TDbConn, query: TSqlQuery, 
              args: openarray[string]): bool =
  ## tries to execute the query and returns true if successful, false otherwise.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  result = PQresultStatus(res) == PGRES_COMMAND_OK
  PQclear(res)

proc Exec*(db: TDbConn, query: TSqlQuery, args: openarray[string]) =
  ## executes the query and raises EDB if not successful.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  if PQresultStatus(res) != PGRES_COMMAND_OK: dbError(db)
  PQclear(res)
  
proc newRow(L: int): TRow =
  newSeq(result, L)
  for i in 0..L-1: result[i] = ""
  
proc setupQuery(db: TDbConn, query: TSqlQuery, 
                args: openarray[string]): PPGresult = 
  var q = dbFormat(query, args)
  result = PQExec(db, q)
  if PQresultStatus(result) != PGRES_TUPLES_OK: dbError(db)
  
proc setRow(res: PPGresult, r: var TRow, line, cols: int) =
  for col in 0..cols-1:
    setLen(r[col], 0)
    var x = PQgetvalue(res, line, col)
    add(r[col], x)
  
iterator FastRows*(db: TDbConn, query: TSqlQuery,
                   args: openarray[string]): TRow =
  ## executes the query and iterates over the result dataset. This is very 
  ## fast, but potenially dangerous: If the for-loop-body executes another
  ## query, the results can be undefined. For Postgres it is safe though.
  var res = setupQuery(db, query, args)
  var L = int(PQnfields(res))
  var result = newRow(L)
  for i in 0..PQntuples(res)-1:
    setRow(res, result, i, L)
    yield result
  PQclear(res)

proc GetAllRows*(db: TDbConn, query: TSqlQuery, 
                 args: openarray[string]): seq[TRow] =
  ## executes the query and returns the whole result dataset.
  result = @[]
  for r in FastRows(db, query, args):
    result.add(r)

iterator Rows*(db: TDbConn, query: TSqlQuery, 
               args: openarray[string]): TRow =
  ## same as `FastRows`, but slower and safe.
  for r in items(GetAllRows(db, query, args)): yield r

proc GetValue*(db: TDbConn, query: TSqlQuery, 
               args: openarray[string]): string = 
  ## executes the query and returns the result dataset's the first column 
  ## of the first row. Returns "" if the dataset contains no rows.
  var x = PQgetvalue(setupQuery(db, query, args), 0, 0)
  result = if isNil(x): "" else: $x
  
proc TryInsertID*(db: TDbConn, query: TSqlQuery, 
                  args: openarray[string]): int64 =
  ## executes the query (typically "INSERT") and returns the 
  ## generated ID for the row or -1 in case of an error. For Postgre this adds
  ## ``RETURNING id`` to the query, so it only works if your primary key is
  ## named ``id``. 
  var val = GetValue(db, TSqlQuery(string(query) & " RETURNING id"), args)
  if val.len > 0:
    result = ParseBiggestInt(val)
  else:
    result = -1

proc InsertID*(db: TDbConn, query: TSqlQuery, 
               args: openArray[string]): int64 = 
  ## executes the query (typically "INSERT") and returns the 
  ## generated ID for the row. For Postgre this adds
  ## ``RETURNING id`` to the query, so it only works if your primary key is
  ## named ``id``. 
  result = TryInsertID(db, query, args)
  if result < 0: dbError(db)
  
proc ExecAffectedRows*(db: TDbConn, query: TSqlQuery, 
                       args: openArray[string]): int64 = 
  ## executes the query (typically "UPDATE") and returns the
  ## number of affected rows.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  if PQresultStatus(res) != PGRES_COMMAND_OK: dbError(db)
  result = parseBiggestInt($PQcmdTuples(res))
  PQclear(res)

proc Close*(db: TDbConn) = 
  ## closes the database connection.
  if db != nil: PQfinish(db)

proc Open*(connection, user, password, database: string): TDbConn =
  ## opens a database connection. Raises `EDb` if the connection could not
  ## be established.
  result = PQsetdbLogin(nil, nil, nil, nil, database, user, password)
  if PQStatus(result) != CONNECTION_OK: dbError(result) # result = nil


