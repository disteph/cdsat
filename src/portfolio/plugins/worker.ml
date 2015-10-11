open Async.Std

open Kernel
open Top.Messages
open Top.Specs
open Theories_register

open LoadPluginsTh

type _ msg = Msg: ('tset,'msg) thanswer -> 'tset msg

let make (type tset) from_pl to_pl (Signed(hdl,init): tset sslot_machine) tset =
  let rec aux (Output(msg,cont)) =
    match msg with
    | None -> return (add cont None) >>= aux
    | Some msg ->
      Pipe.write to_pl (Msg(ThAns(hdl,msg)))
      >>= fun () -> 
      Pipe.read from_pl
      >>= function
      | `Eof -> return ()
      | `Ok(Msg(ThAns(_,msg))) -> match msg with
        | ThProvable _         -> return ()
        | ThNotProvable _      -> aux(add cont None)
        | ThStraight(tset,_)   -> aux(add cont (Some tset))
        | ThAnd(tset1,tset2,j) -> failwith "Cannot treat And branching yet"
        | ThOr(tset1,tset2,j)  -> failwith "Cannot treat Or branching yet"

  and add (type sign) (cont: (sign,tset) slot_machine) = let module Cont = (val cont) in Cont.add
    
  in aux (add init tset)
