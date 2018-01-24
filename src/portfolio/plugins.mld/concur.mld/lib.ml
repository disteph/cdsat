open Async

open General

let stdin = Reader.create(Fd.stdin())
let pause() = Reader.read_char stdin

let stdout = Writer.create(Fd.stdout())
let print s = Writer.write stdout (s^"\n"); Writer.flushed stdout

let write t msg = 
  (* Print.print ["lib",1] (fun p -> p "Trying to write"); *)
  (* Pipe.pushback t *)
  (* >>| fun () -> *)
  if Pipe.is_closed t
  then return(Print.print ["lib",1] (fun p -> p "Pipe died before I could write"))
  else (Print.print ["lib",1] (fun p -> p "Succeeded writing");
        Pipe.write(* _without_pushback *) t msg)

let read from ?(onkill=return) f =
  match%bind Pipe.read from with
  | `Eof -> onkill()
  | `Ok msg -> f msg

                 
let rec dispatch from l =
  let aux msg =
    Deferred.all_unit
      ((dispatch from l)::(List.map (fun t -> write t msg) l))
  in
  let kill_all() = return(List.iter Pipe.close l)
  in
  read from ~onkill:kill_all aux
