open Async

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register
       
module type Extra = sig

  type sassign
  type assign
  type _ t
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :            ack answer
                | Say : _ t     -> say answer
                | Try : sassign -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
                                  
  type msg2th =
    | MsgStraight of assign*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
    | KillYourself of unsat t * sassign * sassign option

  val print2th_in_fmt : Format.formatter -> msg2th -> unit
                                                            
end

module type WhiteBoardExt = sig
  include Export.WhiteBoard
  open DS
  include Extra with type 'a t := 'a t
                              and type assign := Assign.t
                                             and type sassign := SAssign.t
  type 'a ioutput = ('a, Assign.t,(Term.datatype,Value.t) sassign) output
  type 'a islot_machine = ('a, Assign.t,(Term.datatype,Value.t) sassign) slot_machine
  type isslot_machine   = (Assign.t*(Term.datatype,Value.t) sassign) PluginsTh.PluginTh.sslot_machine
end
