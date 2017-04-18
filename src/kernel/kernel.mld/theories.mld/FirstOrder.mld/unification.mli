(********************************************)
(* This is where unification is implemented *)
(********************************************)

open Format

open Top
open Basic
open Variables

module IU = Unifier

(* This module is the datastructure establishing the correspondence
   between meta-variables and their keys.
   Some keys do not correspond to metas (as indicated by function is_meta.
   We call them "wild keys".
*)

module MKcorr : sig
  type t [@@deriving show]
  val empty: t
  val get_key : t -> Meta.t -> IU.keys
  val get_meta: t -> IU.keys -> Meta.t
  val is_meta : t -> IU.keys -> bool
  val add   : t -> Meta.t -> IU.keys -> t
  val remove: t -> Meta.t -> t
  val fold: (Meta.t -> IU.keys -> 'b -> 'b) -> t -> 'b -> 'b
end


(* This module is the datastructure establishing the correspondence
   between the two sides of unification problems
   mk1: correspondence between meta and keys for side 1
   mk2: reverse map
   loc12: map from side1 wild keys to side2 wild keys
   loc21: reverse map
*)

module KKcorr : sig
  type t
  val init: MKcorr.t -> MKcorr.t -> t
  val swap: t -> t
  val get12: t -> IU.keys -> IU.keys option
  val get21: t -> IU.keys -> IU.keys option
  val add: t -> IU.keys -> IU.keys -> t
end


module Unification : sig
  val combine: ('l * 'm) list -> 'l list * 'm list -> ('l * 'm) list
  val unif: bool -> World.t -> MKcorr.t -> MKcorr.t -> IU.t -> IU.t -> (IU.values * IU.values) list -> (IU.t * IU.t) option
end
