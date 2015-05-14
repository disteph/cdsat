(**********)
(* Worlds *)
(**********)

(* A world is a bunch of eigenvariables, a bunch of meta-variables,
   and how they depend on each other *)

(* Module type of sequents' arities: specifies types for
   eigenvariables, meta-variables, and for the datastructures
   recording which eigens and which metas have been created and how
   they depend on each other *)

(* module type ArityType = sig *)
(*   type eigen *)
(*   type meta *)
(*   type t *)
(*   val init     : t *)
(*   val newEigen : t -> eigen*t *)
(*   val newMeta  : t -> meta*t *)
(*   val print_in_fmt: Format.formatter -> t -> unit *)
(* end *)

open Basic

module FreeVar : Interfaces_basic.PHCons
val asIntSort : FreeVar.t -> IntSort.t

type t = private {
  next_eigen : int;
  next_meta  : int;
  ith : FreeVar.t IntMap.t;
  dependencies : int IntMap.t;
}

val init  : t
val liftE : Sorts.t -> t -> FreeVar.t*t
val liftM : Sorts.t -> t -> FreeVar.t*t
val projE : t -> FreeVar.t*t
val projM : t -> FreeVar.t*t

val equal   : t -> t -> bool
val prefix  : t -> t -> bool
val print_in_fmt: Format.formatter -> t -> unit

val print_in_fmtEM : Format.formatter -> t -> unit
val print_in_fmtME : Format.formatter -> t -> unit

