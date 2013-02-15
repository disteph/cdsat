open Lib

module type CollectImplemExt =
sig
  type e
  type t
  val is_empty : t -> bool
  val is_in : e -> t -> bool
  val empty : t
  val add : e -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val remove : e -> t -> t
  val next : t -> e * t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val sub : bool -> t -> t -> e option -> (unit, e) Sums.almost
  val first_diff : t -> t -> e option * bool
end

module PATMapExt
  (Atom: Formulae.AtomType)
  (F: Formulae.FormulaImplem with type lit = Atom.t)
  (FSet: CollectImplemExt with type e = F.t)
  (ASet: CollectImplemExt with type e = Atom.t)
  (V: sig type values val vcompare:values->values->int end)
  : sig

    module UT : sig
      type keys   = ASet.t * FSet.t
      type common = ASet.t * FSet.t
      type branching = (F.lit,F.t) Sums.sum
    end

    type 'a pat =
      | Empty
      | Leaf of (ASet.t * FSet.t) * V.values
      | Branch of UT.common * UT.branching * 'a * 'a
    type t = {
      reveal : t pat;
      id : int;
      info : unit;
    }
    val equal : t -> t -> bool
    val hash : t -> int
    val reveal : t -> t pat
    val id : t -> int
    val info : t -> unit
    val info_gen : t pat -> unit
    val build : t pat -> t
    val clear : unit -> unit
    val compare : t -> t -> int
    val is_empty : t -> bool
    val mem : UT.keys -> t -> bool
    val find : UT.keys -> t -> V.values
    val cardinal : t -> int
    val empty : t
    val leaf : (ASet.t * FSet.t) * V.values -> t
    val branch : UT.common * UT.branching * t * t -> t
    val join : UT.common * t * UT.common * t -> t
    val remove_aux : (UT.keys -> V.values -> t) -> UT.keys -> t -> t
    val remove : UT.keys -> t -> t
    val add :
      ('a -> V.values option -> V.values) -> UT.keys -> 'a -> t -> t
    val merge : (V.values -> V.values -> V.values) -> t * t -> t
    val union : (V.values -> V.values -> V.values) -> t -> t -> t
    val inter : (V.values -> V.values -> V.values) -> t -> t -> t
    val subset : (V.values -> V.values -> bool) -> t -> t -> bool
    val diff : (UT.keys -> V.values -> V.values -> t) -> t -> t -> t
    val aux_and :
      (bool -> (unit, 'a) Sums.almost) ->
      (bool -> (unit, 'a) Sums.almost) ->
      bool -> (unit, 'a) Sums.almost
    val opt_st : ('a -> 'b -> int) -> 'a option * 'b option -> int
    val first_diff :
      (UT.keys -> V.values -> V.values -> 'a option * bool) ->
      ('a -> 'a -> int) ->
      (t -> 'a option) -> t -> t -> 'a option * bool
    val iter : ((ASet.t * FSet.t) -> V.values -> unit) -> t -> unit
    val map : ((ASet.t * FSet.t) -> V.values -> V.values) -> t -> t
    val fold : ((ASet.t * FSet.t) -> V.values -> 'a -> 'a) -> t -> 'a -> 'a
    val choose : t -> (ASet.t * FSet.t) * V.values
    val make :
      ('a -> V.values option -> V.values) -> (UT.keys * 'a) list -> t
    val elements : t -> ((ASet.t * FSet.t) * V.values) list
    val toString :
      ((UT.common -> string) * (UT.branching -> string)) option ->
      ((ASet.t * FSet.t) * V.values -> string) -> t -> string
    val find_su :
      (UT.keys -> V.values -> 'a) ->
      (UT.keys -> V.values -> UT.branching -> 'b) ->
      'b ->
      ('b -> 'b -> 'b) ->
      (UT.common ->
         UT.common ->
           UT.branching option -> ('c, UT.branching) Sums.almost) ->
      bool ->
      (UT.branching -> bool) ->
      ('b -> bool) -> UT.common -> t -> ('a, 'b) Sums.sum
    val sub :
      bool -> UT.common -> UT.common ->
      UT.branching option -> (unit, UT.branching) Sums.almost
    val sup :
      bool -> UT.common -> UT.common ->
      UT.branching option -> (unit, UT.branching) Sums.almost
    val byes : 'a -> 'b -> 'b
    val bempty : ASet.t * FSet.t
    val bsingleton :
      'a -> 'b -> (ASet.e, FSet.e) Sums.sum -> ASet.t * FSet.t
    val bunion :
      ASet.t * FSet.t -> ASet.t * FSet.t -> ASet.t * FSet.t
    val find_sub :
      bool -> ASet.t * FSet.t -> t -> (V.values, ASet.t * FSet.t) Sums.sum
    val find_sup :
      bool -> UT.common -> t -> (V.values, ASet.t * FSet.t) Sums.sum
  end
