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

  type msg2pl = Msg: 'sign Sig.t*('sign,TSet.t,'msg) thsays -> msg2pl

  type msg2th =
    | MsgStraight of TSet.t
    | MsgBranch of TSet.t * TSet.t * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  let make from_pl to_pl (Signed(hdl,init): TSet.t sslot_machine) tset =

    let rec aux from_pl to_pl (Output(msg,cont)) =
      match msg with
      | None -> aux from_pl to_pl (add cont None)
      | Some msg ->
         Deferred.all_unit 
           [ if not(Pipe.is_closed to_pl)
             then Pipe.write to_pl (Msg(hdl,msg)) 
             else return();

             Pipe.read from_pl
             >>= function
             | `Eof -> return ()
             | `Ok(MsgStraight tset) -> aux from_pl to_pl (add cont (Some tset))
             | `Ok(MsgBranch(tset1,tset2,newreader,newwriter))
               -> let Output(_,newcont) = clone cont in
                  Deferred.all_unit [aux from_pl to_pl (add cont (Some tset1));
                                     aux newreader newwriter (add newcont (Some tset2))]
           ]

    and add (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.add
    and clone (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.clone()

                                                                                          
    in aux from_pl to_pl (add init tset)

end
