type
  TEffect* {.compilerproc.} = object of TObject ## \
    ## base effect class; each effect should
    ## inherit from `TEffect` unless you know what
    ## you're doing.
  FTime* = object of TEffect   ## Time effect.
  FIO* = object of TEffect     ## IO effect.
  FReadIO* = object of FIO     ## Effect describing a read IO operation.
  FWriteIO* = object of FIO    ## Effect describing a write IO operation.
  FExecIO* = object of FIO     ## Effect describing an executing IO operation.
