open Async.Std
open Kernel
open Combo
open Theories_register
       
module type Type = sig

  include WhiteBoard
  open DS
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack : ack answer | Say : _ t -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
                                  
  type msg2th =
    | MsgStraight of TSet.t*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  val print2th_in_fmt : Format.formatter -> msg2th -> unit
                                                            
end
       
module Make(WB: WhiteBoard) : sig
  open WB.DS
         
  type ack = private AckL
  type say = private MsgL

  type _ answer = Ack : ack answer | Say : _ WB.t -> say answer
    
  type msg2pl = Msg : Handlers.t option * _ answer * int -> msg2pl
                                  
  type msg2th =
    | MsgStraight of TSet.t*int
    | MsgBranch of (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)
                   * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  val print2th_in_fmt : Format.formatter -> msg2th -> unit

end
