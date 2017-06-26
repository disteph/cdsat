open Async

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register
open Interfaces
                             
module Make(WB: Export.WhiteBoard) = struct

  open WB
  open DS
  type datatypes = Term.datatype*Value.t*Assign.t
  type assign = Assign.t
  type sassign = Term.t * Value.t Top.Values.t [@@deriving show]
                                  
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :            ack answer
                | Say : _ t     -> say answer
                | Try : sassign -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
      
  type msg2th =
    | MsgStraight of sassign*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
    | KillYourself of unsat t * sassign * sassign option

  let print2th_in_fmt fmt = function
    | MsgStraight(assign,chrono)
      -> Format.fprintf fmt "MsgStraight_%i %a" chrono pp_sassign assign
    | MsgBranch(_,_,_,_)
      -> Format.fprintf fmt "MsgBranch"
    | KillYourself(_,_,_)
      -> Format.fprintf fmt "KillYourself"

  type 'a ioutput       = ('a, datatypes) output
  type 'a islot_machine = ('a, datatypes) slot_machine
  type isslot_machine   = datatypes PluginsTh.PluginTh.sslot_machine
end
