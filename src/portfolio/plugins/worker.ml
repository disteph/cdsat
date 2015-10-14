open Async.Std

open Kernel
open Top.Messages
open Top.Specs
open Theories_register
open Combo

open LoadPluginsTh

module Make(WB: WhiteBoard) = struct

  open WB
  open DS

  type msg = Msg: 'sign Sig.t*('sign,TSet.t,'msg) thsays -> msg

  let make from_pl to_pl (Signed(hdl,init): TSet.t sslot_machine) tset =

    let rec aux (Output(msg,cont)) =
      match msg with
      | None -> aux (add cont None)
      | Some msg ->
         Pipe.write to_pl (Msg(hdl,msg))
         >>= fun () -> 
        Pipe.read from_pl
         >>= function
         | `Eof -> return ()
         | `Ok(Msg(_,msg))        -> match msg with
           | ThProvable _         -> return ()
           | ThNotProvable _      -> aux(add cont None)
           | ThStraight(tset,_)   -> aux(add cont (Some tset))
           | ThAnd(tset1,tset2,j) -> failwith "Cannot treat And branching yet"
           | ThOr(tset1,tset2,j)  -> failwith "Cannot treat Or branching yet"

    and add (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.add
                                                                                          
    in aux (add init tset)

end
