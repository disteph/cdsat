(** This module contains the basic constructions of Patricia trees to represent
  maps and sets *)

open Sums

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
  val info_build : infos * (keys -> values -> infos) * (infos -> infos -> infos)

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

module PATMap :
  functor (D : Dest) ->
    functor
      (I:Intern with type keys=D.keys) ->
      sig
        module BackOffice :
          sig
            type 'a pat =
                Empty
              | Leaf of D.keys * D.values
              | Branch of I.common * I.branching * 'a * 'a
            module PATPrimitive :
              sig
                type t = { reveal : t pat; id : int; info : D.infos; }
                val equal : t -> t -> bool
                val hash : t -> int
              end
            type t =
              PATPrimitive.t = {
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
            module H :
              sig
                type key = PATPrimitive.t
                type 'a t = 'a Hashtbl.Make(PATPrimitive).t
                val create : int -> 'a t
                val clear : 'a t -> unit
                val copy : 'a t -> 'a t
                val add : 'a t -> key -> 'a -> unit
                val remove : 'a t -> key -> unit
                val find : 'a t -> key -> 'a
                val find_all : 'a t -> key -> 'a list
                val replace : 'a t -> key -> 'a -> unit
                val mem : 'a t -> key -> bool
                val iter : (key -> 'a -> unit) -> 'a t -> unit
                val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
                val length : 'a t -> int
              end
            val table : t H.t
            val uniq : int ref
            val build : t pat -> t
            val clear : unit -> unit
            val compare : t -> t -> int
            val is_empty : t -> bool
            val mem : I.keys -> t -> bool
            val find : I.keys -> t -> D.values
            val cardinal : t -> int
            val empty : t
            val leaf : D.keys * D.values -> t
            val branch : I.common * I.branching * t * t -> t
            val join : I.common * t * I.common * t -> t
            val remove_aux : (I.keys -> D.values -> t) -> I.keys -> t -> t
            val remove : I.keys -> t -> t
            val toString_aux :
              ((I.common -> string) * (I.branching -> string)) option ->
              (D.keys * D.values -> string) -> t -> string
          end
        type 'a pat =
          'a BackOffice.pat =
            Empty
          | Leaf of D.keys * D.values
          | Branch of I.common * I.branching * 'a * 'a
        module PATPrimitive :
          sig
            type t =
              BackOffice.PATPrimitive.t = {
              reveal : t pat;
              id : int;
              info : D.infos;
            }
            val equal : t -> t -> bool
            val hash : t -> int
          end
        type t =
          PATPrimitive.t = {
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
        module H :
          sig
            type key = PATPrimitive.t
            type 'a t = 'a Hashtbl.Make(PATPrimitive).t
            val create : int -> 'a t
            val clear : 'a t -> unit
            val copy : 'a t -> 'a t
            val add : 'a t -> key -> 'a -> unit
            val remove : 'a t -> key -> unit
            val find : 'a t -> key -> 'a
            val find_all : 'a t -> key -> 'a list
            val replace : 'a t -> key -> 'a -> unit
            val mem : 'a t -> key -> bool
            val iter : (key -> 'a -> unit) -> 'a t -> unit
            val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
            val length : 'a t -> int
          end
        val table : t H.t
        val uniq : int ref
        val build : t pat -> t
        val clear : unit -> unit
        val compare : t -> t -> int
        val is_empty : t -> bool
        val mem : I.keys -> t -> bool
        val find : I.keys -> t -> D.values
        val cardinal : t -> int
        val empty : t
        val leaf : D.keys * D.values -> t
        val branch : I.common * I.branching * t * t -> t
        val join : I.common * t * I.common * t -> t
        val remove_aux : (I.keys -> D.values -> t) -> I.keys -> t -> t
        val remove : I.keys -> t -> t
        val toString_aux :
          ((I.common -> string) * (I.branching -> string)) option ->
          (D.keys * D.values -> string) -> t -> string
        val add :
          ('a -> D.values option -> D.values) -> I.keys -> 'a -> t -> t
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
        val toString :
          ((I.common -> string) * (I.branching -> string)) option ->
          (D.keys * D.values -> string) -> t -> string
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
module PATSet :
  functor
    (D:Dest with type values = unit) ->
    functor
      (I:Intern with type keys=D.keys) ->
      sig
        module PM :
          sig
            module BackOffice :
              sig
                type 'a pat =
                    Empty
                  | Leaf of D.keys * unit
                  | Branch of I.common * I.branching * 'a * 'a
                module PATPrimitive :
                  sig
                    type t = { reveal : t pat; id : int; info : D.infos; }
                    val equal : t -> t -> bool
                    val hash : t -> int
                  end
                type t =
                  PATPrimitive.t = {
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
                module H :
                  sig
                    type key = PATPrimitive.t
                    type 'a t = 'a Hashtbl.Make(PATPrimitive).t
                    val create : int -> 'a t
                    val clear : 'a t -> unit
                    val copy : 'a t -> 'a t
                    val add : 'a t -> key -> 'a -> unit
                    val remove : 'a t -> key -> unit
                    val find : 'a t -> key -> 'a
                    val find_all : 'a t -> key -> 'a list
                    val replace : 'a t -> key -> 'a -> unit
                    val mem : 'a t -> key -> bool
                    val iter : (key -> 'a -> unit) -> 'a t -> unit
                    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
                    val length : 'a t -> int
                  end
                val table : t H.t
                val uniq : int ref
                val build : t pat -> t
                val clear : unit -> unit
                val compare : t -> t -> int
                val is_empty : t -> bool
                val mem : I.keys -> t -> bool
                val find : I.keys -> t -> unit
                val cardinal : t -> int
                val empty : t
                val leaf : D.keys * unit -> t
                val branch : I.common * I.branching * t * t -> t
                val join : I.common * t * I.common * t -> t
                val remove_aux : (I.keys -> unit -> t) -> I.keys -> t -> t
                val remove : I.keys -> t -> t
                val toString_aux :
                  ((I.common -> string) * (I.branching -> string)) option ->
                  (D.keys * unit -> string) -> t -> string
              end
            type 'a pat =
              'a BackOffice.pat =
                Empty
              | Leaf of D.keys * unit
              | Branch of I.common * I.branching * 'a * 'a
            module PATPrimitive :
              sig
                type t =
                  BackOffice.PATPrimitive.t = {
                  reveal : t pat;
                  id : int;
                  info : D.infos;
                }
                val equal : t -> t -> bool
                val hash : t -> int
              end
            type t =
              PATPrimitive.t = {
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
            module H :
              sig
                type key = PATPrimitive.t
                type 'a t = 'a Hashtbl.Make(PATPrimitive).t
                val create : int -> 'a t
                val clear : 'a t -> unit
                val copy : 'a t -> 'a t
                val add : 'a t -> key -> 'a -> unit
                val remove : 'a t -> key -> unit
                val find : 'a t -> key -> 'a
                val find_all : 'a t -> key -> 'a list
                val replace : 'a t -> key -> 'a -> unit
                val mem : 'a t -> key -> bool
                val iter : (key -> 'a -> unit) -> 'a t -> unit
                val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
                val length : 'a t -> int
              end
            val table : t H.t
            val uniq : int ref
            val build : t pat -> t
            val clear : unit -> unit
            val compare : t -> t -> int
            val is_empty : t -> bool
            val mem : I.keys -> t -> bool
            val find : I.keys -> t -> unit
            val cardinal : t -> int
            val empty : t
            val leaf : D.keys * unit -> t
            val branch : I.common * I.branching * t * t -> t
            val join : I.common * t * I.common * t -> t
            val remove_aux : (I.keys -> unit -> t) -> I.keys -> t -> t
            val remove : I.keys -> t -> t
            val toString_aux :
              ((I.common -> string) * (I.branching -> string)) option ->
              (D.keys * unit -> string) -> t -> string
            val add : ('a -> unit option -> unit) -> I.keys -> 'a -> t -> t
            val merge : (unit -> unit -> unit) -> t * t -> t
            val union : (unit -> unit -> unit) -> t -> t -> t
            val inter : (unit -> unit -> unit) -> t -> t -> t
            val subset : (unit -> unit -> bool) -> t -> t -> bool
            val diff : (I.keys -> unit -> unit -> t) -> t -> t -> t
            val aux_and :
              (bool -> (unit, 'a) almost) ->
              (bool -> (unit, 'a) almost) -> bool -> (unit, 'a) almost
            val sub :
              (bool -> I.keys -> unit -> unit option -> (unit, 'a) almost) ->
              (t -> t pat) -> bool -> t -> t -> (unit, 'a) almost
            val opt_st : ('a -> 'b -> int) -> 'a option * 'b option -> int
            val first_diff :
              (I.keys -> unit -> unit -> 'a option * bool) ->
              ('a -> 'a -> int) ->
              (t -> 'a option) -> t -> t -> 'a option * bool
            val iter : (D.keys -> unit -> unit) -> t -> unit
            val map : (D.keys -> unit -> unit) -> t -> t
            val fold : (D.keys -> unit -> 'a -> 'a) -> t -> 'a -> 'a
            val choose : t -> D.keys * unit
            val make : ('a -> unit option -> unit) -> (I.keys * 'a) list -> t
            val elements : t -> (D.keys * unit) list
            val toString :
              ((I.common -> string) * (I.branching -> string)) option ->
              (D.keys * unit -> string) -> t -> string
            val find_su :
              (I.keys -> unit -> 'a) ->
              (I.keys -> unit -> I.branching -> 'b) ->
              'b ->
              ('b -> 'b -> 'b) ->
              (I.common ->
               I.common -> I.branching option -> ('c, I.branching) almost) ->
              bool ->
              (I.branching -> bool) ->
              ('b -> bool) -> I.common -> t -> ('a, 'b) sum
          end
        type 'a pat =
          'a PM.BackOffice.pat =
            Empty
          | Leaf of D.keys * unit
          | Branch of I.common * I.branching * 'a * 'a
        module PATPrimitive :
          sig
            type t =
              PM.BackOffice.PATPrimitive.t = {
              reveal : t pat;
              id : int;
              info : D.infos;
            }
            val equal : t -> t -> bool
            val hash : t -> int
          end
        type t =
          PATPrimitive.t = {
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
        module H :
          sig
            type key = PATPrimitive.t
            type 'a t = 'a Hashtbl.Make(PATPrimitive).t
            val create : int -> 'a t
            val clear : 'a t -> unit
            val copy : 'a t -> 'a t
            val add : 'a t -> key -> 'a -> unit
            val remove : 'a t -> key -> unit
            val find : 'a t -> key -> 'a
            val find_all : 'a t -> key -> 'a list
            val replace : 'a t -> key -> 'a -> unit
            val mem : 'a t -> key -> bool
            val iter : (key -> 'a -> unit) -> 'a t -> unit
            val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
            val length : 'a t -> int
          end
        val table : t H.t
        val uniq : int ref
        val build : t pat -> t
        val clear : unit -> unit
        val compare : t -> t -> int
        val is_empty : t -> bool
        val mem : I.keys -> t -> bool
        val find : I.keys -> t -> unit
        val cardinal : t -> int
        val empty : t
        val leaf : D.keys * unit -> t
        val branch : I.common * I.branching * t * t -> t
        val join : I.common * t * I.common * t -> t
        val remove_aux : (I.keys -> unit -> t) -> I.keys -> t -> t
        val remove : I.keys -> t -> t
        val toString_aux :
          ((I.common -> string) * (I.branching -> string)) option ->
          (D.keys * unit -> string) -> t -> string
        val singleton : D.keys -> PM.t
        val add : I.keys -> PM.t -> PM.t
        val union : PM.t -> PM.t -> PM.t
        val inter : PM.t -> PM.t -> PM.t
        val subset : PM.t -> PM.t -> bool
        val diff : PM.t -> PM.t -> PM.t
        val first_diff :
          (PM.t -> D.keys option) -> PM.t -> PM.t -> D.keys option * bool
        val sub :
          (PM.t -> PM.t PM.pat) ->
          bool -> PM.t -> PM.t -> (unit, I.keys) almost
        val iter : (D.keys -> unit) -> PM.t -> unit
        val map : (D.keys -> unit) -> PM.t -> PM.t
        val fold : (D.keys -> 'a -> 'a) -> PM.t -> 'a -> 'a
        val choose : PM.t -> D.keys
        val elements : PM.t -> D.keys list
        val find_su :
          (I.keys -> 'a) ->
          (I.keys -> I.branching -> 'b) ->
          'b ->
          ('b -> 'b -> 'b) ->
          (I.common ->
           I.common -> I.branching option -> ('c, I.branching) almost) ->
          bool ->
          (I.branching -> bool) ->
          ('b -> bool) -> I.common -> PM.t -> ('a, 'b) sum
        val toString :
          ((I.common -> string) * (I.branching -> string)) option ->
          (D.keys -> string) -> t -> string
        val make : I.keys list -> PM.t
        val for_all : (D.keys -> bool) -> t -> bool
        val exists : (D.keys -> bool) -> t -> bool
        val filter : (D.keys -> bool) -> t -> t
        val partition : (I.keys -> bool) -> t -> PM.t * PM.t
        val elect : (D.keys -> D.keys -> D.keys) -> t -> D.keys
      end
val empty_info_build : unit * ('a -> 'b -> unit) * ('c -> 'd -> unit)
type 'a mmc_infos = 'a option * 'a option * int
val mmc_info_build :
  ('a -> 'b) ->
  ('c option * 'd option * int) * ('e -> 'f -> 'e option * 'e option * int) *
  ('a option * 'a option * int ->
   'a option * 'a option * int -> 'a option * 'a option * int)
type 'a m_infos = 'a option
val splmin : ('a -> 'a -> int) -> 'a option -> 'a option -> 'a option
type 'a mm_infos = ('a * 'a option) option
val dblmin :
  ('a -> 'a -> int) ->
  ('a * 'a option) option ->
  ('a * 'a option) option -> ('a * 'a option) option
