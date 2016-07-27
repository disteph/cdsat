open Async.Std

let stdin = Reader.create(Fd.stdin())
let pause() = Reader.read_char stdin

let stdout = Writer.create(Fd.stdout())
let print s = Writer.write stdout (s^"\n"); Writer.flushed stdout

let write t msg =
  Dump.print ["lib",1] (fun p -> p "Trying to write");
  Pipe.pushback t
  >>| fun () ->
  if Pipe.is_closed t
  then Dump.print ["lib",1] (fun p -> p "Pipe died before I could write")
  else (Dump.print ["lib",1] (fun p -> p "Succeeded writing");
        Pipe.write_without_pushback t msg)
