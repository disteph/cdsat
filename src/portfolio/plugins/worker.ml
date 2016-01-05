open Async.Std
open Lib

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

  let print_in_fmt fmt (Msg(hdl,msg)) =
    Format.fprintf fmt "%a says %a" Sig.print_in_fmt hdl (print_msg_in_fmt TSet.print_in_fmt) msg

  type msg2th =
    | MsgStraight of TSet.t
    | MsgBranch of TSet.t * TSet.t * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  let print_in_fmt2 fmt = function
    | MsgStraight tset 
      -> Format.fprintf fmt "MsgStraight %a" TSet.print_in_fmt tset
    | MsgBranch(tset1,tset2,_,_)
      -> Format.fprintf fmt "MsgBranch(%a, %a)" TSet.print_in_fmt tset1 TSet.print_in_fmt tset2

  let add   (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.add
  let clone (type sign) (cont: (sign,TSet.t) slot_machine) = let module Cont = (val cont) in Cont.clone()

  let make from_pl to_pl (Signed(hdl,init)) tset =

    let read from_pl f =
      (* Dump.msg (Some(fun p-> p "%a wants to read" Sig.print_in_fmt hdl)) None None; *)
      Pipe.read from_pl
      >>= function
      | `Eof -> return()
      | `Ok msg ->
         Dump.msg (Some(fun p-> p "%a reads %a" Sig.print_in_fmt hdl print_in_fmt2 msg)) None None;
        f msg
    in

    let rec flush reader writer msg =
      (* Dump.msg (Some(fun p-> p "%a enters flush" Sig.print_in_fmt hdl)) None None; *)
      read reader (function
      | MsgStraight _ -> flush reader writer msg
      | MsgBranch(_,_,newreader,newwriter) -> 
         Deferred.all_unit
           [ Lib.write newwriter (Msg(hdl,msg));
             flush newreader newwriter msg;
             flush reader writer msg
           ]
      )
    in

    let rec loop from_pl to_pl (Output(output_msg,cont)) =

      let keep_on() = read from_pl (function
        | MsgStraight tset
          -> loop from_pl to_pl (add cont (Some tset))
        | MsgBranch(tset1,tset2,newreader,newwriter)
          -> let Output(_,newcont) = clone cont in
             Deferred.all_unit
               [loop from_pl   to_pl     (add cont    (Some tset1));
                loop newreader newwriter (add newcont (Some tset2))]
      )
      in

      (* Dump.msg (Some(fun p-> p "%a looks at its output_msg" Sig.print_in_fmt hdl)) None None; *)

      match output_msg with
      | None -> 
         Dump.msg (Some(fun p-> p "%a: no output msg" Sig.print_in_fmt hdl)) None None;
        loop from_pl to_pl (add cont None)
      | Some msg ->
         (* print (Dump.toString (fun p-> p "Outputting message %a" print_in_fmt (Msg(hdl,msg)))) >>= fun () ->  *)
         Deferred.all_unit 
           [ Lib.write to_pl (Msg(hdl,msg));
             match msg with
             | ThProvable _ -> flush from_pl to_pl msg
             | _            -> keep_on()
           ]
                                                                                          
    in loop from_pl to_pl (add init tset)

end
