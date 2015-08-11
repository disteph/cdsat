(****************************************)
(* Internal representation of a Unifier *)
(****************************************)

open Format

open Top
open Basic
open Variables

open General.Patricia
open General.SetConstructions

exception AlreadyConstrained

(* Private datatypes for unifiers *)

type keys
type values

val kcompare     : keys -> keys -> int
val get_sort     : keys -> Sorts.t

(* Constructing values *)

val key2val      : keys    -> values
val eigen2val    : Eigen.t -> values
val bC           : Symbols.t -> values list -> values

val internalise  : (Meta.t -> keys) -> (FreeVar.t,_) Terms.term -> values

(* Actual unifiers *)

type t

val empty        : t
val add          : keys -> values -> t -> t
val new_key      : Sorts.t -> t -> keys*t

(* Exposing values into readable stuff, with on-the-fly normalisation of unifier *)

type exposed = 
| Eigen of Eigen.t
| Key of keys
| C of Symbols.t*(values list)

val expose       : values*t  -> exposed * t
val get          : keys -> t -> (values * t) option

(* Printing out stuff *)

val print_in_fmtK: Format.formatter -> keys   -> unit
val print_in_fmtV: Format.formatter -> values -> unit
val print_in_fmt : Format.formatter -> t      -> unit

(* Maps between keys and metas *)

module KMap  : PATMap.S with type keys = keys   and type values = Meta.t
module MMap  : PATMap.S with type keys = Meta.t and type values = keys
module KKMap : PATMap.S with type keys = keys   and type values = keys
