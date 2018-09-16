(**********************)
(* Single Assignments *)
(**********************)

open General
open HCons
open Patricia
open Patricia_tools

open Basic
open Terms

type ('t,'v) sassign = SAssign : ('t * ('v,_) Values.t) -> ('t,'v) sassign
[@@unboxed] (* The constructor takes 1 argument (as a pair), not two *)
[@@deriving eq,ord,hash,show]

val is_Boolean : _ sassign -> bool
                                    
type ('t,'v) bassign = 't * ('v,bool) Values.t [@@deriving eq,ord,hash,show]
val negation : ('t,'v) bassign -> ('t,'v) bassign

module BAssign : sig
  type t = (Term.t,Values.Value.t) bassign [@@deriving eq,ord,hash,show]
  val id : t -> int
end

(* HConsed version of single assignments *)

module SAssign : sig
  include PHCons
  val reveal : t -> (Term.t, Values.Value.t) sassign
  val build      : (Term.t * (Values.Value.t,_) Values.t) -> t
  val boolassign : ?b:bool -> Term.t -> t
  val is_Boolean : t -> bool
end

module Assign : sig
  include Set.S_H with type e = SAssign.t
                   and type common = int
                   and type branching = int
  val pp : t Format.printer 
  val show : t -> string
  val next : t -> e*t
end
