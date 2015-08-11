open Format

open Top
open Interfaces_basic
open Basic
open Specs

module LitF : sig
  include PHCons
  type revealed = bool*int
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end

module BoundVar : sig
  include Interfaces_basic.PHCons
  val build: int*Sorts.t -> t
  val get_sort: t -> Sorts.t
  val get_from_context: t -> (int -> 'a) -> 'a
end

module TermB: Terms.S with type leaf = IntSort.t

type termB = (BoundVar.t,unit) Terms.term

module LitB : sig
  include PHCons
  type revealed = bool*TermB.t
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end
