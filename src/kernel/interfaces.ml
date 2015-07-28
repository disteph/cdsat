open Top.Messages
open Register

module type WhiteBoard = sig
  module DS : Top.Specs.GTheoryDSType
  open DS
  type answer = private Provable of TSet.t | NotProvable of TSet.t
  type planswer = 
  | PlProvable    : (TSet.t,thProvable) thanswer -> planswer
  | PlNotProvable : TSet.t*((TSet.t,thNotProvable) thanswer list) -> planswer
  val check : planswer -> answer
end
