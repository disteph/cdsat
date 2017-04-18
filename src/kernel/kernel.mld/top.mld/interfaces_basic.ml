(******************************************************************)
(* This file contains basic module types that specify how the kernel
interacts with the other components of Psyche *)
(******************************************************************)

(* Generic interface for printable hashed types *)

module type PH = sig
  type t [@@deriving eq,ord,show,hash]
end

(* Generic interface for printable hconsed types *)

module type PHCons = sig
  include PH
  val id: t -> int
  val clear: unit->unit
end

(* Collection Interface that Theory needs to provide for Kernel *)

module type Collection = sig
  type e
  type t [@@deriving eq,show]
  val empty: t
  val singleton: e -> t
  val add  : e -> t -> t
  val remove: e -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val is_empty : t -> bool
  val mem      : e -> t -> bool
  val equal    : t -> t -> bool
  val subset   : t -> t -> bool
  val next     : t -> e*t
  val fold     : (e -> 'a -> 'a) -> t -> 'a -> 'a
end

(* Type of Monads *)

module type MonadType = sig
  type 'a t 
  val return : 'a -> 'a t
  val bind   : ('a -> 'b t) -> 'a t -> 'b t
end
