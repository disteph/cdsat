open Core.Std
open Async.Std

open Kernel
open Top
open Messages
open Register
open Combo
open Types

type _ msg = Msg: ('tset,'msg) thanswer -> 'tset msg


let make (type tset)
    (from_pl : tset msg Pipe.Reader.t)
    (to_pl : tset msg Pipe.Writer.t)
    : tset slot_machine -> unit Deferred.t
    =
  let rec aux (sm: tset slot_machine) =
    let SM(msg,cont) = sm in
    match msg with
    | None -> return (cont None) >>= aux
    | Some msg ->
      Pipe.write to_pl (Msg msg)
      >>= fun () -> 
      Pipe.read from_pl
      >>= function
      | `Eof -> return ()
      | `Ok(Msg(ThAns(_,msg) as thans)) -> match msg with
        | ThProvable _         -> return ()
        | ThNotProvable _      -> aux(cont None)
        | ThStraight(_,_)      -> aux(cont(Some thans))
        | ThAnd(tset1,tset2,j) -> failwith "Cannot treat And branching yet"
        | ThOr(tset1,tset2,j)  -> failwith "Cannot treat Or branching yet"
  in aux
