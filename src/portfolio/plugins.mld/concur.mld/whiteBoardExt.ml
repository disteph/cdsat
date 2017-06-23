open Async

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register
open Interfaces
                             
module Make(WB: Export.WhiteBoard) = struct

  open WB
  open DS

  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :           ack answer
                | Say : _ t    -> say answer
                | Try : Term.t -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
      
  type msg2th =
    | MsgStraight of Assign.t*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
    | KillYourself of unsat t * Term.t * Term.t option

  let print2th_in_fmt fmt = function
    | MsgStraight(assign,chrono)
      -> Format.fprintf fmt "MsgStraight_%i %a" chrono Assign.pp assign
    | MsgBranch(_,_,_,_)
      -> Format.fprintf fmt "MsgBranch"
    | KillYourself(_,_,_)
      -> Format.fprintf fmt "KillYourself"

  type 'a ioutput = ('a, Assign.t,(Term.datatype,Value.t) sassign) output
  type 'a islot_machine = ('a, Assign.t,(Term.datatype,Value.t) sassign) slot_machine
  type isslot_machine   = (Assign.t*(Term.datatype,Value.t) sassign) PluginsTh.PluginTh.sslot_machine
end
