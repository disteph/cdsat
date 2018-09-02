(**********)
(* Timers *)
(**********)

type t = string*(float ref)*(float option ref)

let newtimer s = 
  let timed   = ref 0. in
  let ongoing = ref None in
  (s,timed,ongoing)

let name (s,_,_) = s

let isRunning (s,timed,ongoing) =
  match ongoing with 
  | Some _ -> true
  | None   -> false

let start (s,timed,ongoing) =
  match !ongoing with
  | Some last -> failwith("Trying to start timer "^s^" but it is already started")
  | None      -> ongoing := Some(Sys.time())

let stop (s,timed,ongoing) =
  match !ongoing with
  | Some last -> let span = Sys.time()-.last in
    timed := !timed+.span;ongoing := None
  | None -> failwith("Trying to stop timer "^s^" but it is already stopped")

let watch (s,timed,ongoing) =
  match !ongoing with
  | Some last -> let span = Sys.time()-.last in !timed+.span
  | None -> !timed

let reset (s,timed,ongoing) =
  match !ongoing with
  | Some last -> timed:=0.;ongoing:=Some(Sys.time())
  | None -> timed:=0.

let transfer t t' = stop t; start t'
