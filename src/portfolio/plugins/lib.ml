open Async.Std

let stdin = Reader.create(Fd.stdin())
let pause() = Reader.read_char stdin

let stdout = Writer.create(Fd.stdout())
let print s = Writer.write stdout (s^"\n"); Writer.flushed stdout

let write t msg =
  Pipe.pushback t
  >>| fun () ->
  if Pipe.is_closed t
  then ()
  else Pipe.write_without_pushback t msg
