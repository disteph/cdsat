open Interfaces_basic
open Basic

(*************)
(* Variables *)
(*************)

(* Specifying types for eigenvariables, meta-variables, and free
   variables *)

module Meta : sig
  include PHCons
  val get_sort: t -> Sorts.t
end

module Eigen: sig
  include PHCons
  val get_sort: t -> Sorts.t
end

module FreeVar : sig
  include PHCons
  val get_sort: t -> Sorts.t
  type freeVarExposed = Meta of Meta.t | Eigen of Eigen.t
  val reveal : t -> freeVarExposed
end


(**********)
(* Worlds *)
(**********)

(* A world is a bunch of eigenvariables, a bunch of meta-variables,
   and how they depend on each other. The fold function ranges over
   all the meta-variables that existed when a given eigen was
   created *)

module World : sig

  include PHCons
  val ppEM : Format.formatter -> t -> unit
  val ppME : Format.formatter -> t -> unit

  val init  : t
  val liftE : Sorts.t -> t -> FreeVar.t*t
  val liftM : Sorts.t -> t -> FreeVar.t*t
  val proj  : t -> FreeVar.t*t

  val equal   : t -> t -> bool
  val hash    : t -> int
  val prefix  : t -> t -> bool

  val fold: t -> Eigen.t -> (Meta.t -> 'a -> 'a) -> 'a -> 'a
end

(* Module used to check whether a bunch of free variables make sense
   in a certain world *)

module MakesSense : sig
  type t
  val init  : t
  val fv    : FreeVar.t -> t
  val combine : t -> t -> t
  val check : t -> World.t -> bool
end
