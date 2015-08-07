open Format

open Top
open Interfaces_basic
open Basic
open Specs

module LitF : sig
  include PHCons
  type revealed = bool*IntSort.t
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end

module TermB: Terms.S with type leaf = IntSort.t

module LitB : sig
  include PHCons
  type revealed = bool*TermB.t
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end
