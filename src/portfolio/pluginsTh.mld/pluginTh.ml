open Kernel
open Top.Terms
open Top.Sassigns
open Top.Messages

open Combo
open Theories.Register
open Theories.Theory

open Tools

(* Here is the API that we expect from a theory plugin *)

(* An output of a plugin query should be silence or a message *)
    
type _ output =
  | Silence
  | Msg: ('s,_) message -> 's output
  (* | Try: sassign -> _ output *)

(* A slot machine represents a state of the theory plugin.
   The state offers some function for e.g. transitioning to a new state. *)

type _ slot_machine =
    SlotMachine : {
      (* Possibly given a new trail assignment to take into account,
         returns an output and a new state, in the form of a slot machine. *)
      add     : SAssign.t option -> 's output * 's slot_machine;
      (* Please propose a certain number of assignments (with probabilities),
         we are particularly interested in getting a value for term *)
      propose : ?term: Term.t -> int -> (SAssign.t * float) list;
      (* Here are some new shared terms. Speak up to say if e.g. you can endorse
         any equality imposed on shared terms, or else.  *)
      share   : TSet.t -> 's output * 's slot_machine;
      (* Branching point: please provide a clone of your slot machine for the
         newly created branch.  *)
      clone   : unit -> 's slot_machine;
      (* You previously asked to monitor some constraints for something to
         happen to them. Well something definitely happened to that one. *)
      watched : Constraint.t -> 's slot_machine;
      (* Kill yourself: this assignment on the trail was found to be a conflict. *)
      suicide : Assign.t -> unit
    } -> 's slot_machine

(* Signed version of the above *)

type sslot_machine = Signed: ('sign*_) Tags.t * 'sign slot_machine -> sslot_machine

(* The plugin API must provide an initial state and a clearing function *)

type 'sign pluginTh = {
    init : 'sign slot_machine;
    clear: unit -> unit
  }

(* The final plugin API *)

module type Type = sig
  type sign (* Type used for signing messages (typically defined in the kernel) *)
  type api  (* Kernel API for plugin *)
  val hdl : (sign*api) Tags.t (* Theory handler *)
  module Make(W: Writable) : sig (* Given a way to construct terms... *)
    val make : api -> sign pluginTh (*...and given an implementation of the kernel API
                                      ...please provide initial state & clear function *)
  end
end

(* A fail state that can be used in any theory plugin *)
let fail_state =
  let add _ = failwith "Are you dumb? I already told you it was provable" in
  let share, clone, suicide, watched = add, add, add, add in
  let propose ?term i = add i in
  SlotMachine { add; propose; share; clone; watched; suicide }
