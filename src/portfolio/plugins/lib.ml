open Async.Std

let stdin = Reader.create(Fd.stdin())
let pause() = Reader.read_char stdin

let stdout = Writer.create(Fd.stdout())
let print s = Writer.write stdout (s^"\n"); Writer.flushed stdout
