open Top
open Terms
open Sassigns

module type Type = sig
  val ds : dsKey list
  type api
  val make : (module Writable) -> api
  val name : string
  (* sign is the secret type used by the theory module to label its messages *)
  type sign
end

(* Standard API that a theory module, kernel-side,
   may offer to the plugins piloting it. *)

type _ output =
   | Silence
   | Msg: ('s,_) Messages.message -> 's output
   (* | Try: sassign -> _ output *)

type _ slot_machine =
  SlotMachine : {
      add     : SAssign.t option -> 's output * 's slot_machine;
      propose : ?term: Term.t -> int -> (SAssign.t * float) list;
      share   : TSet.t -> 's output * 's slot_machine;
      clone   : unit -> 's slot_machine;
      suicide : Assign.t -> unit
    } -> 's slot_machine
