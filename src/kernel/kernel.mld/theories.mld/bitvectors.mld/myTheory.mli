open Top
open Terms
open Messages
open Sassigns

open Theory

module V = Bv_value
  
type sign

type valuation = Eq.MyTheory.sign Eq.Valuation.signed
val vkey : V.t Values.Key.t

module type API = sig
  (* States of the algorithm *)
  type state
  (* Initial state of the algorithm *)
  val init: state

  (* Direct evaluation functions for bit-vector terms and formulae *)
  (* val term_eval : (Term.t -> V.t) -> Term.t -> V.t
   * val form_eval : (Term.t -> V.t) -> Term.t -> bool *)

  (* Evaluation function for a formula, given a valuation.
     Either raises an exception CannotEval,
     or produces a message Propa(assign,boolassign), where
     boolassign is the Boolean assignment saying if the formula evaluates to true or false
     assign are the assignments that were used in the evaluation *)
  val eval : valuation -> Term.t -> (sign, straight) message * int
end

val hdl : (sign*(module API)) Tags.t
