#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## `Actor`:idx: support for Nimrod. An actor is implemented as a thread with
## a channel as its inbox. This module requires the ``--threads:on``
## command line switch.

from os import sleep

type
  TTask*[TIn, TOut] = object{.pure, final.}
    when TOut isnot void:
      receiver*: ptr TChannel[TOut] ## the receiver channel of the response
    action*: proc (x: TIn): TOut {.thread.} ## action to execute;
                                            ## sometimes useful
    shutDown*: bool ## set to tell an actor to shut-down
    data*: TIn ## the data to process

  TActor[TIn, TOut] = object{.pure, final.}
    i: TChannel[TTask[TIn, TOut]]
    t: TThread[ptr TActor[TIn, TOut]]
    
  PActor*[TIn, TOut] = ptr TActor[TIn, TOut] ## an actor
  
proc spawn*[TIn, TOut](action: proc(
    self: PActor[TIn, TOut]){.thread.}): PActor[TIn, TOut] =
  ## creates an actor; that is a thread with an inbox. The caller MUST call
  ## ``join`` because that also frees the actor's associated resources.
  result = cast[PActor[TIn, TOut]](allocShared0(sizeof(result[])))
  open(result.i)
  createThread(result.t, action, result)

proc inbox*[TIn, TOut](self: PActor[TIn, TOut]): ptr TChannel[TIn] =
  ## gets a pointer to the associated inbox of the actor `self`.
  result = addr(self.i)

proc running*[TIn, TOut](a: PActor[TIn, TOut]) =
  ## returns true if the actor `a` is running.
  result = running(a.t)

proc ready*[TIn, TOut](a: PActor[TIn, TOut]): bool =
  ## returns true if the actor `a` is ready to process new messages.
  result = ready(a.i)

proc join*[TIn, TOut](a: PActor[TIn, TOut]) =
  ## joins an actor.
  joinThread(a.t)
  close(a.i)
  deallocShared(a)

proc recv*[TIn, TOut](a: PActor[TIn, TOut]): TTask[TIn, TOut] =
  ## receives a task from `a`'s inbox.
  result = recv(a.i)

proc send*[TIn, TOut, X, Y](receiver: PActor[TIn, TOut], msg: TIn,
                            sender: PActor[X, Y]) =
  ## sends a message to `a`'s inbox.
  var t: TTask[TIn, TOut]
  t.receiver = addr(sender.i)
  shallowCopy(t.data, msg)
  send(receiver.i, t)

proc send*[TIn, TOut](receiver: PActor[TIn, TOut], msg: TIn, 
                      sender: ptr TChannel[TOut] = nil) =
  ## sends a message to `receiver`'s inbox.
  var t: TTask[TIn, TOut]
  t.receiver = sender
  shallowCopy(t.data, msg)
  send(receiver.i, t)

proc sendShutdown*[TIn, TOut](receiver: PActor[TIn, TOut]) =
  ## send a shutdown message to `receiver`.
  var t: TTask[TIn, TOut]
  t.shutdown = true
  send(receiver.i, t)

proc reply*[TIn, TOut](t: TTask[TIn, TOut], m: TOut) =
  ## sends a message to io's output message box.
  when TOut is void:
    {.error: "you cannot reply to a void outbox".}
  assert t.receiver != nil
  send(t.receiver[], m)


# ----------------- actor pools ----------------------------------------------

type
  TActorPool*[TIn, TOut] = object{.pure, final.}  ## an actor pool
    actors: seq[PActor[TIn, TOut]]
    when TOut isnot void:
      outputs: TChannel[TOut]

proc `^`*[T](f: ptr TChannel[T]): T =
  ## alias for 'recv'.
  result = recv(f[])

proc poolWorker[TIn, TOut](self: PActor[TIn, TOut]) {.thread.} =
  while true:
    var m = self.recv
    if m.shutDown: break
    when TOut is void:
      m.action(m.data)
    else:
      self.repy(m.action(m.data))

proc createActorPool*[TIn, TOut](a: var TActorPool[TIn, TOut], poolSize = 4) =
  ## creates an actor pool.
  newSeq(a.actors, poolSize)
  when TOut isnot void:
    open(a.outputs)
  for i in 0 .. < a.actors.len:
    a.actors[i] = spawn(poolWorker[TIn, TOut])

proc sync*[TIn, TOut](a: var TActorPool[TIn, TOut], polling=50) =
  ## waits for every actor of `a` to finish with its work. Currently this is
  ## implemented as polling every `polling` ms. This will change in a later
  ## version, however.
  while true:
    var wait = false
    for i in 0..high(a.actors):
      if not a.actors[i].i.ready: 
        wait = true
        break
    if not wait: break
    sleep(polling)

proc terminate*[TIn, TOut](a: var TActorPool[TIn, TOut]) =
  ## terminates each actor in the actor pool `a` and frees the
  ## resources attached to `a`.
  var t: TTask[TIn, TOut]
  t.shutdown = true
  for i in 0.. <a.actors.len: send(a.actors[i].i, t)
  for i in 0.. <a.actors.len: join(a.actors[i])
  when TOut isnot void:
    close(a.outputs)
  a.actors = nil

proc join*[TIn, TOut](a: var TActorPool[TIn, TOut]) =
  ## short-cut for `sync` and then `terminate`.
  sync(a)
  terminate(a)

template setupTask =
  t.action = action
  shallowCopy(t.data, input)

template schedule =
  # extremely simple scheduler: We always try the first thread first, so that
  # it remains 'hot' ;-). Round-robin hurts for keeping threads hot.
  for i in 0..high(p.actors):
    if p.actors[i].i.ready:
      p.actors[i].i.send(t)
      return
  # no thread ready :-( --> send message to the thread which has the least
  # messages pending:
  var minIdx = -1
  var minVal = high(int)
  for i in 0..high(p.actors):
    var curr = p.actors[i].i.peek
    if curr == 0:
      # ok, is ready now:
      p.actors[i].i.send(t)
      return
    if curr < minVal and curr >= 0:
      minVal = curr
      minIdx = i
  if minIdx >= 0:
    p.actors[minIdx].i.send(t)
  else:
    raise newException(EDeadThread, "cannot send message; thread died")

proc spawn*[TIn, TOut](p: var TActorPool[TIn, TOut], input: TIn,
                       action: proc (input: TIn): TOut {.thread.}
                       ): ptr TChannel[TOut] =
  ## uses the actor pool to run ``action(input)`` concurrently.
  ## `spawn` is guaranteed to not block.
  var t: TTask[TIn, TOut]
  setupTask()
  result = addr(p.outputs)
  t.receiver = result
  schedule()

proc spawn*[TIn](p: var TActorPool[TIn, void], input: TIn,
                 action: proc (input: TIn) {.thread.}) =
  ## uses the actor pool to run ``action(input)`` concurrently.
  ## `spawn` is guaranteed to not block.
  var t: TTask[TIn, void]
  setupTask()
  schedule()
  
when isMainModule:
  var
    a: TActorPool[int, void]
  createActorPool(a)
  for i in 0 .. < 300:
    a.spawn(i, proc (x: int) {.thread.} = echo x)

  when false:
    proc treeDepth(n: PNode): int {.thread.} =
      var x = a.spawn(treeDepth, n.le)
      var y = a.spawn(treeDepth, n.ri)
      result = max(^x, ^y) + 1

  a.join()


