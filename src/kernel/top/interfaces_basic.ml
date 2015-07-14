(******************************************************************)
(* This file contains basic module types that specify how the kernel
interacts with the other components of Psyche *)
(******************************************************************)

(* Generic interface for printable hconsed types *)

module type PHCons = sig
  type t
  val id: t -> int
  val print_in_fmt: Format.formatter -> t -> unit
  val clear: unit->unit
  val compare : t -> t -> int
end

(* Collection Interface that Plugin needs to provide for Kernel *)

module type CollectExtra = sig
  type e
  type t
  val empty: t
  val add  : e -> t -> t
  val remove: e -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
end

(* Collection Interface that Theory needs to provide for Kernel *)

module type CollectTrusted = sig
  include CollectExtra
  val is_empty : t -> bool
  val mem      : e -> t -> bool
  val subset   : t -> t -> bool
  val next     : t -> e*t
  val fold     : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val print_in_fmt: Format.formatter -> t -> unit
end

(* Collection Interface that Kernel manipulates *)

module type CollectKernel = sig
  type ts
  type ps
  include CollectTrusted
  val forTrusted: t -> ts
  val forPlugin : t -> ps
  val recons : ts -> t
end

(* Type of Monads *)

module type MonadType = sig
  type 'a t 
  val return : 'a -> 'a t
  val bind   : ('a -> 'b t) -> 'a t -> 'b t
end
