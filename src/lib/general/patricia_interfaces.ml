(*********************************************************************)
(* Module types defining the interface of the Patricia tries library
   to represent maps and sets *)
(*********************************************************************)

open Sums

(* Direct type of Patricia tries. 'a is the type of the sub-tries *)

type ('a,'keys,'values,'common,'branching) poly_rev =
  | Empty
  | Leaf of 'keys * 'values
  | Branch of 'common * 'branching * 'a * 'a

(* We embark extra info in Patricia tries. How to construct this info
   is given as a record of this type *)
      
type ('keys,'values,'infos) info_build_type = 
  {empty_info  : 'infos;
   leaf_info   : 'keys -> 'values -> 'infos;
   branch_info : 'infos -> 'infos -> 'infos}

(* The two interfaces Dest and Intern descibe the material that must be
  provided to construct a Patricia tree structure.

  Dest describes the info that that the user expects to provide anyway to build
  a map/set.
  Intern describes the structures to be used for the internal mechanisms of
  Patricia trees; standard implementations of Intern are the object of module
  SetConstructions *)

module type MapDestType = sig

  (* Domain of the map/set (keys)*)
  type keys
  val kcompare : keys -> keys -> int

  (* Co-domain of the map (values), set it to unit for a set *)    

  type values

  (* Allows to store information about the Patricia tree: typically, number of
    bindings stored, etc *) 
  type infos

  (* Provides info for empty tree, singleton tree, and disjoint union
    of two tree *)
  val info_build : (keys,values,infos) info_build_type

  (* Do you want the patricia trees hconsed? if so you should provide
    an equal function for values, and hash functions for keys and values *) 

  val treeHCons : ((keys->int)
                   *(values->int)
                   *(values -> values -> bool)) option
end


module type Intern = sig
  (* Implementation of keys and how to compute them.
    Typically for a HConsed keys type, common = int *)

  type keys
  type common
  val tag : keys -> common

  (* Branching is the type of data used for discriminating the keys (themselves
    represented in common via tag) *) 

  type branching
  val bcompare : branching -> branching -> int

  (* Check discriminates its first argument over second *)
  val check : common -> branching -> bool

  (* Given two elements of common, disagree outputs: their common part, the
    first branching data that discriminates them a boolean saying whether that
    data was in the first [true] or second [false] argument *)
  val disagree : common -> common -> common * branching * bool

  (* Checks whether the first argument is compatible with the second up to some
    branching data. Should output true if the first two arguments are equal *)
  val match_prefix : common -> common -> branching -> bool
end

module type PATMapType = sig
  type keys
  type values
  type common
  type branching
  type infos

  type ('v,'i) param
  type t = (values,infos) param
  type pat = (t,keys,values,common,branching) poly_rev

  val equal  : t -> t -> bool
  val hash   : t -> int
  val reveal : t -> pat
  val id     : t -> int
  val info   : t -> infos
  val clear    : unit -> unit
  val compare  : t -> t -> int
  val is_empty : t -> bool
    (* val checktree : branching list -> t -> bool *)
  val mem      : keys -> t -> bool
  val find     : keys -> t -> values
  val cardinal : t -> int
  val empty    : t
  val singleton: keys -> values -> t
  val remove : keys -> t -> t
  val add    : keys -> (values option -> values) -> t -> t
  type ('v1,'i1,'v2,'i2,'a) merge = {
      sameleaf  : keys -> 'v1 -> 'v2 -> 'a;
      emptyfull : ('v2,'i2) param -> 'a;
      fullempty : ('v1,'i1) param -> 'a;
      combine   : 'a -> 'a -> 'a
    }
  val merge_poly : ('v1,'i1,'v2,'i2,'a) merge
                   -> ('v1,'i1) param
                   -> ('v2,'i2) param
                   -> 'a
  val merge      : ?equal:(('v,'i)param -> 'a)
                   -> ('v,'i,'v,'i,'a) merge
                   -> ('v,'i) param
                   -> ('v,'i) param
                   -> 'a
  val union_poly : (keys -> 'v1 -> 'v2 -> values)
                    -> (('v1,'i1) param -> t)
                    -> (('v2,'i2) param -> t)
                    -> ('v1,'i1) param
                    -> ('v2,'i2) param
                    -> t
  val union      : (values -> values -> values) -> t -> t -> t
  val inter      : (keys -> values -> values -> values) -> t -> t -> t
  val inter_poly : (keys -> 'v1 -> 'v2 -> values)  -> ('v1,_)param -> ('v2,_)param -> t
  val diff       : (keys -> values -> values -> t) -> t -> t -> t
  val diff_poly  : (keys -> values -> 'v -> t) -> t -> ('v,_)param  -> t
  val subset     : (values -> values -> bool)  -> t -> t -> bool
  val sub :
    (bool -> keys -> values -> values option -> (unit, 'a) almost) ->
    (t -> t) -> bool -> t -> t -> (unit, 'a) almost
  val first_diff :
    (keys -> values -> values -> 'a option * bool) ->
    ('a -> 'a -> int) -> (t -> 'a option) -> t -> t -> 'a option * bool
  val iter   : (keys -> 'v -> unit) -> ('v,_)param -> unit
  val map    : (keys -> 'v -> values) -> ('v,_)param -> t
  val fold   : (keys -> 'v -> 'a -> 'a) -> ('v,_)param -> 'a -> 'a
  val choose : t -> keys * values
  val make :
    ('a -> values option -> values) -> (keys * 'a) list -> t
  val elements : t -> (keys * values) list
  val print_in_fmt: 
    ?tree:((Format.formatter -> common -> unit) * (Format.formatter -> branching -> unit))
    -> (Format.formatter -> (keys * values) -> unit)
    -> Format.formatter -> t -> unit
  val find_su :
    (keys -> values -> 'a) ->
    (keys -> values -> branching -> 'b) ->
    'b ->
    ('b -> 'b -> 'b) ->
    (common ->
     common -> branching option -> ('c, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) -> common -> t -> ('a, 'b) sum
end


module type SetDestType = sig
  (* Domain of the map/set (keys)*)

  type keys
  val kcompare : keys -> keys -> int

  (* Allows to store information about the Patricia tree: typically, number of
    bindings stored, etc *) 
  type infos

  (* Provides info for empty tree, singleton tree, and disjoint union
    of two tree *)
  val info_build : (keys,unit,infos) info_build_type

  (* Do you want the patricia trees hconsed? if so you should provide
  a hash function for keys *) 
  val treeHCons : (keys->int) option 

end

module type PATSetType = sig
  type e
  type common
  type branching
  type infos

  type ('v,'i) param
  type t = (unit,infos) param
  type pat = (t,e,unit,common,branching) poly_rev
  val equal  : t -> t -> bool
  val hash   : t -> int
  val reveal : t -> pat
  val id     : t -> int
  val info   : t -> infos
  val clear  : unit -> unit
  val compare  : t -> t -> int
  val is_empty : t -> bool
    (* val checktree : branching list -> t -> bool *)
  val mem    : e -> t -> bool
  val find   : e -> t -> unit
  val cardinal : t -> int
  val empty  : t
  val remove : e -> t -> t
  val singleton : e -> t
  val add    : e -> t -> t
  val union  : t -> t -> t
  val inter  : t -> t -> t
  val inter_poly : t -> (_,_)param -> t
  val subset : t -> t -> bool
  val diff   : t -> t -> t
  val diff_poly : t -> (_,_)param -> t
  val first_diff :
    (t -> e option) -> t -> t -> e option * bool
  val sub :
    (t -> t) ->
    bool -> t -> t -> (unit, e) almost
  val iter   : (e -> unit) -> t -> unit
  (* val map    : (e -> unit) -> t -> t *)
  val fold   : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val choose : t -> e
  val elements : t -> e list
  val find_su :
    (e -> 'a) ->
    (e -> branching -> 'b) ->
    'b ->
    ('b -> 'b -> 'b) ->
    (common ->
     common -> branching option -> ('c, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) -> common -> t -> ('a, 'b) sum
  val print_in_fmt: 
    ?tree:((Format.formatter -> common -> unit) * (Format.formatter -> branching -> unit))
    -> (Format.formatter -> e -> unit)
    -> Format.formatter -> t -> unit
  val make    : e list -> t
  val for_all : (e -> bool) -> t -> bool
  val exists  : (e -> bool) -> t -> bool
  val filter  : (e -> bool) -> t -> t
  val partition : (e -> bool) -> t -> t * t
  val elect   : (e -> e -> e) -> t -> e
end
