open Async.Std
open Kernel.Combo

module type Type = sig

  include WhiteBoard
  open DS
         
  type msg2pl = Msg : _ t -> msg2pl
                                  
  type msg2th =
    | MsgStraight of TSet.t
    | MsgBranch of TSet.t * TSet.t * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  val print2th_in_fmt : Format.formatter -> msg2th -> unit
                                                            
end
       
module Make(WB: WhiteBoard) : sig
  open WB.DS
         
  type msg2pl = Msg : _ WB.t -> msg2pl
                                  
  type msg2th =
    | MsgStraight of TSet.t
    | MsgBranch of TSet.t * TSet.t * (msg2th Pipe.Reader.t) * (msg2pl Pipe.Writer.t)

  val print2th_in_fmt : Format.formatter -> msg2th -> unit

end
