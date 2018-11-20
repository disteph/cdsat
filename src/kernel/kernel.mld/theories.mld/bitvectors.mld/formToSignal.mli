(*Contains functions to create HardCaml signals*)    
open Top
open Terms
open Messages
open Sassigns

open Theory

module Sgn : sig
  include module type of Circuit
  val isT : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Ppx_hash_lib.Std.Hash.folder
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
       
module T : sig

  (* Direct evaluation functions for bit-vector terms and formulae *)
  val term_eval : (Term.t -> V.t option) -> Term.t -> Sgn.t
  val form_eval : (Term.t -> V.t option) -> Term.t -> bool

  (* Evaluation function for a formula, given the state of the algorithm.
     Either raises an exception CannotEval,
     or produces a message Propa(assign,boolassign), where
     boolassign is the Boolean assignment saying if the formula evaluates to true or false
     assign are the assignments that were used in the evaluation *)
  val eval : Assign.t -> (Term.t -> V.t option) -> Term.t -> (unit, straight) message

end
