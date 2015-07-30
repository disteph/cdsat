open Async.Std

open Kernel
open Top.Messages
open Theories_register
open Types

type _ msg = Msg: ('tset,'msg) thanswer -> 'tset msg

let make from_pl to_pl =
  let rec aux (SM(msg,cont)) =
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
