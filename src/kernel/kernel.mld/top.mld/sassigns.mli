(**********************)
(* Single Assignments *)
(**********************)

open General
open HCons
open Patricia
open Patricia_tools

open Basic
open Terms

type sassign = SAssign : (Term.t * (Values.Value.t,_) Values.t) -> sassign
[@@unboxed] (* The constructor takes 1 argument (as a pair), not two *)
[@@deriving eq,ord,hash,show]

val is_Boolean : sassign -> bool
                                    
type bassign = Term.t * (Values.Value.t,bool) Values.t [@@deriving eq,ord,hash,show]
val negation : bassign -> bassign
val boolassign : ?b:bool -> Term.t -> sassign

(* HConsed version of single assignments *)

module SAssign : sig
  include SHCons with type revealed = sassign
  val reveal : t -> sassign
end

module Assign : sig
  include Collection with type e = SAssign.t
                      and type t = (SAssign.t, unit, int, int, EmptyInfo.infos*[`HCons]) poly
  val id : t -> int
end
