(** This module contains the basic constructions of Patricia trees to represent
  maps and sets *)

open Sums

type ('keys,'values,'infos) info_build_type = 
  {empty_info  : 'infos;
   leaf_info   : 'keys -> 'values -> 'infos;
   branch_info : 'infos -> 'infos -> 'infos}

(** The two interfaces Dest and Intern descibe the material that must be
  provided to construct a Patricia tree structure.

  Dest describes the info that that the user expects to provide anyway to build
  a map/set.
  Intern describes the structures to be used for the internal mechanisms of
  Patricia trees; standard implementations of Intern are the object of module
  SetConstructions *)

module type Dest = sig
  (** Domain of the map/set (keys)*)

  type keys
  val kcompare : keys -> keys -> int

  (** Co-domain of the map (values), set it to unit for a set *)    

  type values
  val vcompare : values -> values -> int

  (** Allows to store information about the Patricia tree: typically, number of
    bindings stored, etc *) 
  type infos

  (** Provides info for empty tree, singleton tree, and disjoint union
    of two tree *)
  val info_build : (keys,values,infos) info_build_type

  (** Do you want the patricia trees hconsed? *)
  val treeHCons : bool
end

module type Intern = sig
  (** Implementation of keys and how to compute them.
    Typically for a HConsed keys type, common = int *)

  type keys
  type common
  val tag : keys -> common

  (** Branching is the type of data used for discriminating the keys (themselves
    represented in common via tag) *) 

  type branching
  val bcompare : branching -> branching -> int

  (** Check discriminates its first argument over second *)
  val check : common -> branching -> bool

  (** Given two elements of common, disagree outputs: their common part, the
    first branching data that discriminates them a boolean saying whether that
    data was in the first [true] or second [false] argument *)
  val disagree : common -> common -> common * branching * bool

  (** Checks whether the first argument is compatible with the second up to some
    branching data. Should output true if the first two arguments are equal *)
  val match_prefix : common -> common -> branching -> bool
end

module PATMap
  (D : Dest)
  (I:Intern with type keys=D.keys)
  : sig
    type 'a pat =
      | Empty
      | Leaf of D.keys * D.values
      | Branch of I.common * I.branching * 'a * 'a
    type t = {
      reveal : t pat;
      id : int;
      info : D.infos;
    }
    val equal : t -> t -> bool
    val hash : t -> int
    val reveal : t -> t pat
    val id : t -> int
    val info : t -> D.infos
    val info_gen : t pat -> D.infos
    val build : t pat -> t
    val clear : unit -> unit
    val compare : t -> t -> int
    val is_empty : t -> bool
    (* val checktree : I.branching list -> t -> bool *)
    val mem : I.keys -> t -> bool
    val find : I.keys -> t -> D.values
    val cardinal : t -> int
    val empty : t
    val leaf : D.keys * D.values -> t
    val branch : I.common * I.branching * t * t -> t
    val join : I.common * t * I.common * t -> t
    val remove_aux : (I.keys -> D.values -> t) -> I.keys -> t -> t
    val remove : I.keys -> t -> t
    val add : I.keys -> (D.values option -> D.values) -> t -> t
    val merge : (D.values -> D.values -> D.values) -> t * t -> t
    val union : (D.values -> D.values -> D.values) -> t -> t -> t
    val inter : (D.values -> D.values -> D.values) -> t -> t -> t
    val subset : (D.values -> D.values -> bool) -> t -> t -> bool
    val diff : (I.keys -> D.values -> D.values -> t) -> t -> t -> t
    val aux_and :
      (bool -> (unit, 'a) almost) ->
      (bool -> (unit, 'a) almost) -> bool -> (unit, 'a) almost
    val sub :
      (bool -> I.keys -> D.values -> D.values option -> (unit, 'a) almost) ->
      (t -> t pat) -> bool -> t -> t -> (unit, 'a) almost
    val opt_st : ('a -> 'b -> int) -> 'a option * 'b option -> int
    val first_diff :
      (I.keys -> D.values -> D.values -> 'a option * bool) ->
      ('a -> 'a -> int) -> (t -> 'a option) -> t -> t -> 'a option * bool
    val iter : (D.keys -> D.values -> unit) -> t -> unit
    val map : (D.keys -> D.values -> D.values) -> t -> t
    val fold : (D.keys -> D.values -> 'a -> 'a) -> t -> 'a -> 'a
    val choose : t -> D.keys * D.values
    val make :
      ('a -> D.values option -> D.values) -> (I.keys * 'a) list -> t
    val elements : t -> (D.keys * D.values) list
    val print_in_fmt: 
      ((Format.formatter -> I.common -> unit) * (Format.formatter -> I.branching -> unit)) option
      -> (Format.formatter -> (D.keys * D.values) -> unit)
      -> Format.formatter -> t -> unit
    val find_su :
      (I.keys -> D.values -> 'a) ->
      (I.keys -> D.values -> I.branching -> 'b) ->
      'b ->
      ('b -> 'b -> 'b) ->
      (I.common ->
         I.common -> I.branching option -> ('c, I.branching) almost) ->
      bool ->
      (I.branching -> bool) ->
      ('b -> bool) -> I.common -> t -> ('a, 'b) sum
  end

module PATSet
  (D:Dest with type values = unit)
  (I:Intern with type keys=D.keys)
  : sig
    type 'a pat =
      | Empty
      | Leaf of D.keys * unit
      | Branch of I.common * I.branching * 'a * 'a
    type t = {
      reveal : t pat;
      id : int;
      info : D.infos;
    }
    val equal : t -> t -> bool
    val hash : t -> int
    val reveal : t -> t pat
    val id : t -> int
    val info : t -> D.infos
    val info_gen : t pat -> D.infos
    val build : t pat -> t
    val clear : unit -> unit
    val compare : t -> t -> int
    val is_empty : t -> bool
    (* val checktree : I.branching list -> t -> bool *)
    val mem : I.keys -> t -> bool
    val find : I.keys -> t -> unit
    val cardinal : t -> int
    val empty : t
    val remove : I.keys -> t -> t
    val singleton : D.keys -> t
    val add : I.keys -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val subset : t -> t -> bool
    val diff : t -> t -> t
    val first_diff :
      (t -> D.keys option) -> t -> t -> D.keys option * bool
    val sub :
      (t -> t pat) ->
      bool -> t -> t -> (unit, I.keys) almost
    val iter : (D.keys -> unit) -> t -> unit
    val map : (D.keys -> unit) -> t -> t
    val fold : (D.keys -> 'a -> 'a) -> t -> 'a -> 'a
    val choose : t -> D.keys
    val elements : t -> D.keys list
    val find_su :
      (I.keys -> 'a) ->
      (I.keys -> I.branching -> 'b) ->
      'b ->
      ('b -> 'b -> 'b) ->
      (I.common ->
         I.common -> I.branching option -> ('c, I.branching) almost) ->
      bool ->
      (I.branching -> bool) ->
      ('b -> bool) -> I.common -> t -> ('a, 'b) sum
    val print_in_fmt: 
      ((Format.formatter -> I.common -> unit) * (Format.formatter -> I.branching -> unit)) option
      -> (Format.formatter -> D.keys -> unit)
      -> Format.formatter -> t -> unit
    val make : I.keys list -> t
    val for_all : (D.keys -> bool) -> t -> bool
    val exists : (D.keys -> bool) -> t -> bool
    val filter : (D.keys -> bool) -> t -> t
    val partition : (I.keys -> bool) -> t -> t * t
    val elect : (D.keys -> D.keys -> D.keys) -> t -> D.keys
  end

(* Gives the standard inhabitant info_build when info type is unit *)

val empty_info_build : ('keys,'values,unit) info_build_type

(* Info type to record maximum key
   Standard inhabitant info_build when given a comparison function
*)

type 'keys m_infos = 'keys option
val m_info_build :  ('keys -> 'keys -> int) -> ('keys,'values,'keys m_infos) info_build_type

(* Info type to record minimum key, maximum key, and cardinal of table
   Standard inhabitant info_build when given 
*)

type 'keys mmc_infos = 'keys option * 'keys option * int
val mmc_info_build :  ('keys -> 'keys -> int) -> ('keys,'values,'keys mmc_infos) info_build_type

(* Info type to record maximum 2 keys
   Standard inhabitant info_build when given a comparison function
*)

type 'keys mm_infos = ('keys * 'keys option) option
val mm_info_build :  ('keys -> 'keys -> int) -> ('keys,'values,'keys mm_infos) info_build_type
