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
    val toString : t -> string
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val compareE : e -> e -> int
    val sub : bool -> t -> t -> e option -> (unit, e) Patricia.almost
    val first_diff : t -> t -> e option * bool
  end
module PATMapExt :
  functor (F : Formulae.FormulaImplem) ->
    functor
      (FSet : sig
                type e = F.t
                type t
                val is_empty : t -> bool
                val is_in : e -> t -> bool
                val empty : t
                val add : e -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val remove : e -> t -> t
                val next : t -> e * t
                val toString : t -> string
                val hash : t -> int
                val equal : t -> t -> bool
                val compare : t -> t -> int
                val compareE : e -> e -> int
                val sub :
                  bool -> t -> t -> e option -> (unit, e) Patricia.almost
                val first_diff : t -> t -> e option * bool
              end) ->
      functor
        (ASet : sig
                  type e = Formulae.Atom.t
                  type t
                  val is_empty : t -> bool
                  val is_in : e -> t -> bool
                  val empty : t
                  val add : e -> t -> t
                  val union : t -> t -> t
                  val inter : t -> t -> t
                  val remove : e -> t -> t
                  val next : t -> e * t
                  val toString : t -> string
                  val hash : t -> int
                  val equal : t -> t -> bool
                  val compare : t -> t -> int
                  val compareE : e -> e -> int
                  val sub :
                    bool -> t -> t -> e option -> (unit, e) Patricia.almost
                  val first_diff : t -> t -> e option * bool
                end) ->
        functor
          (V : sig type values val vcompare : values -> values -> int end) ->
          sig
            module D :
              sig
                type keys = ASet.t * FSet.t
                val kcompare : ASet.t * FSet.t -> ASet.t * FSet.t -> int
                type values = V.values
                val vcompare : values -> values -> int
                type infos = unit
                val info_build :
                  unit * ('a -> 'b -> unit) * ('c -> 'd -> unit)
                val treeHCons : bool
              end
            module EASet :
              sig
                type e = Formulae.Atom.t
                type t = ASet.t
                val is_empty : t -> bool
                val is_in : e -> t -> bool
                val empty : t
                val add : e -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val remove : e -> t -> t
                val next : t -> e * t
                val toString : t -> string
                val hash : t -> int
                val equal : t -> t -> bool
                val compare : t -> t -> int
                val compareE : e -> e -> int
                val sub :
                  bool -> t -> t -> e option -> (unit, e) Patricia.almost
                val first_diff : t -> t -> e option * bool
                type keys = D.keys
                val tag : 'a * 'b -> 'a
              end
            module EFSet :
              sig
                type e = F.t
                type t = FSet.t
                val is_empty : t -> bool
                val is_in : e -> t -> bool
                val empty : t
                val add : e -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val remove : e -> t -> t
                val next : t -> e * t
                val toString : t -> string
                val hash : t -> int
                val equal : t -> t -> bool
                val compare : t -> t -> int
                val compareE : e -> e -> int
                val sub :
                  bool -> t -> t -> e option -> (unit, e) Patricia.almost
                val first_diff : t -> t -> e option * bool
                type keys = D.keys
                val tag : 'a * 'b -> 'b
              end
            module UT :
              sig
                type keys = SetConstructions.TypesFromCollect(EASet).keys
                type common =
                    SetConstructions.TypesFromCollect(EASet).common *
                    SetConstructions.TypesFromCollect(EFSet).common
                val tag : keys -> common
                type branching =
                    (SetConstructions.TypesFromCollect(EASet).branching,
                     SetConstructions.TypesFromCollect(EFSet).branching)
                    Patricia.sum
                val bcompare : branching -> branching -> int
                val check : common -> branching -> bool
                val disagree : common -> common -> common * branching * bool
                val match_prefix : common -> common -> branching -> bool
                val sub :
                  (bool ->
                   SetConstructions.TypesFromCollect(EASet).common ->
                   SetConstructions.TypesFromCollect(EASet).common ->
                   SetConstructions.TypesFromCollect(EASet).branching option ->
                   (unit, SetConstructions.TypesFromCollect(EASet).branching)
                   Patricia.almost) ->
                  (bool ->
                   SetConstructions.TypesFromCollect(EFSet).common ->
                   SetConstructions.TypesFromCollect(EFSet).common ->
                   SetConstructions.TypesFromCollect(EFSet).branching option ->
                   (unit, SetConstructions.TypesFromCollect(EFSet).branching)
                   Patricia.almost) ->
                  bool ->
                  common ->
                  common ->
                  branching option -> (unit, branching) Patricia.almost
                val pequals :
                  (SetConstructions.TypesFromCollect(EFSet).common ->
                   SetConstructions.TypesFromCollect(EFSet).common -> bool) ->
                  common -> common -> bool
              end
            module BackOffice :
              sig
                type 'a pat =
                  'a Patricia.PATMap(D)(UT).BackOffice.pat =
                    Empty
                  | Leaf of D.keys * D.values
                  | Branch of UT.common * UT.branching * 'a * 'a
                module PATPrimitive :
                  sig
                    type t =
                      Patricia.PATMap(D)(UT).BackOffice.PATPrimitive.t = {
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
                val mem : UT.keys -> t -> bool
                val find : UT.keys -> t -> D.values
                val cardinal : t -> int
                val empty : t
                val leaf : D.keys * D.values -> t
                val branch : UT.common * UT.branching * t * t -> t
                val join : UT.common * t * UT.common * t -> t
                val remove_aux :
                  (UT.keys -> D.values -> t) -> UT.keys -> t -> t
                val remove : UT.keys -> t -> t
                val toString_aux :
                  ((UT.common -> string) * (UT.branching -> string)) option ->
                  (D.keys * D.values -> string) -> t -> string
              end
            type 'a pat =
              'a BackOffice.pat =
                Empty
              | Leaf of D.keys * D.values
              | Branch of UT.common * UT.branching * 'a * 'a
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
            val mem : UT.keys -> t -> bool
            val find : UT.keys -> t -> D.values
            val cardinal : t -> int
            val empty : t
            val leaf : D.keys * D.values -> t
            val branch : UT.common * UT.branching * t * t -> t
            val join : UT.common * t * UT.common * t -> t
            val remove_aux : (UT.keys -> D.values -> t) -> UT.keys -> t -> t
            val remove : UT.keys -> t -> t
            val toString_aux :
              ((UT.common -> string) * (UT.branching -> string)) option ->
              (D.keys * D.values -> string) -> t -> string
            val add :
              ('a -> D.values option -> D.values) -> UT.keys -> 'a -> t -> t
            val merge : (D.values -> D.values -> D.values) -> t * t -> t
            val union : (D.values -> D.values -> D.values) -> t -> t -> t
            val inter : (D.values -> D.values -> D.values) -> t -> t -> t
            val subset : (D.values -> D.values -> bool) -> t -> t -> bool
            val diff : (UT.keys -> D.values -> D.values -> t) -> t -> t -> t
            val aux_and :
              (bool -> (unit, 'a) Patricia.almost) ->
              (bool -> (unit, 'a) Patricia.almost) ->
              bool -> (unit, 'a) Patricia.almost
            val opt_st : ('a -> 'b -> int) -> 'a option * 'b option -> int
            val first_diff :
              (UT.keys -> D.values -> D.values -> 'a option * bool) ->
              ('a -> 'a -> int) ->
              (t -> 'a option) -> t -> t -> 'a option * bool
            val iter : (D.keys -> D.values -> unit) -> t -> unit
            val map : (D.keys -> D.values -> D.values) -> t -> t
            val fold : (D.keys -> D.values -> 'a -> 'a) -> t -> 'a -> 'a
            val choose : t -> D.keys * D.values
            val make :
              ('a -> D.values option -> D.values) -> (UT.keys * 'a) list -> t
            val elements : t -> (D.keys * D.values) list
            val toString :
              ((UT.common -> string) * (UT.branching -> string)) option ->
              (D.keys * D.values -> string) -> t -> string
            val find_su :
              (UT.keys -> D.values -> 'a) ->
              (UT.keys -> D.values -> UT.branching -> 'b) ->
              'b ->
              ('b -> 'b -> 'b) ->
              (UT.common ->
               UT.common ->
               UT.branching option -> ('c, UT.branching) Patricia.almost) ->
              bool ->
              (UT.branching -> bool) ->
              ('b -> bool) -> UT.common -> t -> ('a, 'b) Patricia.sum
            val sub :
              bool ->
              UT.common ->
              UT.common ->
              UT.branching option -> (unit, UT.branching) Patricia.almost
            val sup :
              bool ->
              UT.common ->
              UT.common ->
              UT.branching option -> (unit, UT.branching) Patricia.almost
            val byes : 'a -> 'b -> 'b
            val bempty : ASet.t * FSet.t
            val bsingleton :
              'a -> 'b -> (ASet.e, FSet.e) Patricia.sum -> ASet.t * FSet.t
            val bunion :
              ASet.t * FSet.t -> ASet.t * FSet.t -> ASet.t * FSet.t
            val find_sub :
              bool ->
              SetConstructions.TypesFromCollect(EASet).common *
              SetConstructions.TypesFromCollect(EFSet).common ->
              t -> (D.values, ASet.t * FSet.t) Patricia.sum
            val find_sup :
              bool ->
              UT.common -> t -> (D.values, ASet.t * FSet.t) Patricia.sum
          end
