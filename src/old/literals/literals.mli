open Format

open Top
open Basic
open Specs

module LitF : sig
  type t [@@deriving eq,hash,ord,show]
  val id : t -> int
  val print_in_fmt : ?print_atom:(formatter -> int -> unit)
                     -> formatter -> t -> unit
  val pp : formatter -> t -> unit
  val clear : unit -> unit
  type revealed = bool*int
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end

module LitB : sig
  include PHCons
  type revealed = bool*Terms.TermB.t
  val reveal : t -> revealed
  val build : revealed -> t
  val clear : unit -> unit
  val negation : t -> t
end

module TS : Termstructure.Type with type (_,_) t = LitF.t
