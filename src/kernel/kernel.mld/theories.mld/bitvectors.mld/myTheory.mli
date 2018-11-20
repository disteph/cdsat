open Top
open Terms
open Messages
open Sassigns

open Theory

open BDValues
    
type sign
       
module type API = sig

  (* States of the algorithm *)
  type state
  (* Initial state of the algorithm *)
  val init: state

  (* Direct evaluation functions for bit-vector terms and formulae *)
  val term_eval : (Term.t -> V.t) -> Term.t -> V.t
  val form_eval : (Term.t -> V.t) -> Term.t -> bool

  (* Evaluation function for a formula, given the state of the algorithm.
     Either raises an exception CannotEval,
     or produces a message Propa(assign,boolassign), where
     boolassign is the Boolean assignment saying if the formula evaluates to true or false
     assign are the assignments that were used in the evaluation *)
  val eval : state -> Term.t -> (sign, straight) message

end

val hdl : (sign*(module API)) Tags.t
