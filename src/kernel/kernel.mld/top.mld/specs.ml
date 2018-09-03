(******************)
(* Specifications *)
(******************)

open Format

open General
open Sums
       
open Basic
open Variables
open Terms
open Sassigns

(* Extension of GlobalDS,
   that adds interfacing functions with theory-specific types for terms and values.
     proj can project the global term datatype into the theory-specific one ts
     proj_opt, if the theory module has values, offers an injection (resp. a projection)
   from (resp. to) theory-specific values to (resp. from) global values (the projection
   ends in an option type since a global value may not contain a value for this theory).
*)

type _ has_values  = private HV
type has_no_values = private HNV
       
type (_,_) conv =
  | HasVconv   : { vinj  : 'v -> 'gv ;
                   vproj : 'gv -> 'v option }
                 -> ('v has_values,'gv) conv
  | HasNoVconv : (has_no_values,_) conv

module type DSproj = sig
  type values
  val conv: (values,Values.Value.t) conv
end

(* type version of the above *)
type 'v dsProj = (module DSproj with type values   = 'v)

(* Standard API that a theory module, kernel-side, may offer to the plugins piloting it. *)

type _ output =
   | Silence
   | Msg: ('s,_) Messages.message -> 's output
   (* | Try: sassign -> _ output *)

type _ slot_machine =
  SlotMachine : {
      add     : sassign option -> 's output * 's slot_machine;
      propose : ?term: Term.t -> int -> (sassign * float) list;
      share   : TSet.t -> 's output * 's slot_machine;
      clone   : unit -> 's slot_machine;
      suicide : 'a -> unit
    } -> 's slot_machine
