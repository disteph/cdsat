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

  let add   (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.add
  let clone (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.clone()

  let make from_pl to_pl (Signed(hdl,init)) tset =

    let rec loop from_pl to_pl (Output(output_msg,cont)) =

      let write msg = 
        if not(Pipe.is_closed to_pl)
        then Pipe.write to_pl (Msg(hdl,msg)) 
        else return()
      in

      let read f =
        Pipe.read from_pl
        >>= function
        | `Eof -> return()
        | `Ok msg -> f msg
      in

      let rec flush() = read(fun _ -> flush())
      in

      let keep_on() = read (function
        | MsgStraight tset
          -> loop from_pl to_pl (add cont (Some tset))
        | MsgBranch(tset1,tset2,newreader,newwriter)
          -> let Output(_,newcont) = clone cont in
             Deferred.all_unit
               [loop from_pl to_pl (add cont (Some tset1));
                loop newreader newwriter (add newcont (Some tset2))]
      )
      in
      match output_msg with
      | None -> loop from_pl to_pl (add cont None)
      | Some msg ->
         Deferred.all_unit 
           [ write msg;
             match msg with
             | ThProvable _ -> flush()
             | _ -> keep_on()
           ]
                                                                                          
    in loop from_pl to_pl (add init tset)

end
