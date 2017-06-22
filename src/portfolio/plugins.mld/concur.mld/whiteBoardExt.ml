open Async

open Kernel
open Top.Messages
open Theories.Register
       
module type Extra = sig

  type term
  type assign
  type _ t
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack :         ack answer
                | Say : _ t  -> say answer
                | Try : term -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
                                  
  type msg2th =
    | MsgStraight of assign*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
    | KillYourself of unsat t * term * term option

  val print2th_in_fmt : Format.formatter -> msg2th -> unit
                                                            
end

module type Type = sig
  include Export.WhiteBoard
  include Extra with type 'a t := 'a t and type assign := DS.Assign.t and type term := DS.Term.t
end
                      
module Make(WB: Export.WhiteBoard)
       : (Extra with type 'a t := 'a WB.t and type assign := WB.DS.Assign.t and type term := WB.DS.Term.t)
  = struct

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

end