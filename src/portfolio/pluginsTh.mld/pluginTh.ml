open Kernel
open Top.Terms
open Top.Sassigns
open Top.Messages

open Combo
open Theories.Register
open Theories.Theory

open Tools

type _ output =
  | Silence
  | Msg: ('s,_) message -> 's output
  (* | Try: sassign -> _ output *)

type _ slot_machine =
    SlotMachine : {
      add     : SAssign.t option -> 's output * 's slot_machine;
      propose : ?term: Term.t -> int -> (SAssign.t * float) list;
      share   : TSet.t -> 's output * 's slot_machine;
      clone   : unit -> 's slot_machine;
      watched : Constraint.t -> 's slot_machine;
      suicide : Assign.t -> unit
    } -> 's slot_machine

type sslot_machine = Signed: ('sign*_) Tags.t * 'sign slot_machine -> sslot_machine

type 'sign pluginTh = {
    init : 'sign slot_machine;
    clear: unit -> unit
  }
       
module type Type = sig
  type sign
  type api
  val hdl : (sign*api) Tags.t
  module Make(W: Writable) : sig
    val make : api -> sign pluginTh
  end
end

let fail_state =
  let add _ = failwith "Are you dumb? I already told you it was provable" in
  let share, clone, suicide, watched = add, add, add, add in
  let propose ?term i = add i in
  SlotMachine { add; propose; share; clone; watched; suicide }
