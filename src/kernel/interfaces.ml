open Top.Messages

module type WhiteBoard = sig
  module DS : Top.Specs.GTheoryDSType
  open DS
  type answer = private Provable of TSet.t | NotProvable of TSet.t
  type _ thanswer = ThAns : 'a Register.t * ('a,TSet.t,'b) thsays -> 'b thanswer
  type planswer = 
  | PlProvable    : thProvable thanswer -> planswer
  | PlNotProvable : TSet.t*(thNotProvable thanswer list) -> planswer
  val check : planswer -> answer
end

(* module type InferenceModule = sig *)

(*   type sign *)
(*   type tset *)

(*   type 'a state *)

(*   type slot_machine = *)
(*   | SM: (sign,tset,'msg) thsays option*slot_machine state *)
(*     -> slot_machine *)

(*   val search: tset -> slot_machine *)

(* end *)

  (* type pluginDS *)
  (* val make: pluginDS -> unit *)
