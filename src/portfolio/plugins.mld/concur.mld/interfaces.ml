open Async

open Kernel
open Top
open Messages
open Specs
open Theories.Register
       
module type Extra = sig

  type datatypes
  type sassign [@@deriving show]
  type _ t
         
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

  val print2th_in_fmt : Format.formatter -> msg2th -> unit
                                                            
  type 'a ioutput       = ('a, datatypes) output
  type 'a islot_machine = ('a, datatypes) slot_machine
  type isslot_machine   = datatypes PluginsTh.PluginTh.sslot_machine
end

module type WhiteBoardExt = sig
  include Export.WhiteBoard
  open DS
  include Extra with type 'a t := 'a t
                              and type datatypes = Term.datatype*Value.t*Assign.t
                              and type sassign = Term.t * Value.t Values.t
end
