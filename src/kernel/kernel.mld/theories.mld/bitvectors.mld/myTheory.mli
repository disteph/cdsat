open Top
open Specs
open Messages
open Sassigns
       
type sign

module V : sig
  include HardCaml.Comb.S
  val isT : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Ppx_hash_lib.Std.Hash.folder
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
       
module type API = sig
  type termdata
  type assign
  type tset

  (* States of the algorithm *)
  type state
  (* Initial state of the algorithm *)
  val init: state

  (* Direct evaluation functions for bit-vector terms and formulae *)
  val term_eval : (int -> V.t) -> termdata termF -> V.t
  val form_eval : (int -> V.t) -> termdata termF -> bool

  (* Evaluation function for a formula, given the state of the algorithm.
     Either raises an exception CannotEval,
     or produces a message Propa(assign,boolassign), where
     boolassign is the Boolean assignment saying if the formula evaluates to true or false
     assign are the assignments that were used in the evaluation *)
  val eval : state -> termdata termF
             -> (sign, assign * (termdata termF, _)bassign*tset, straight) message
end


include Theory.Type
        with type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                                   and type assign = 'a
                                                   and type tset   = 's)
