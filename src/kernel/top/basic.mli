(*****************)
(* Basic modules *)
(*****************)

open Format

open Interfaces_basic

module HashedTypeFromHCons(M: sig
                               type t
                               val id: t -> int
                             end) : sig
  include Hashtbl.HashedType with type t = M.t
  val hash_fold_t : t Base__Hash.folder
end
  
module IntSort : sig
  include PHCons
  val reveal : t -> int*Sorts.t
  val build  : int*Sorts.t -> t
  val isDefined  : t -> bool
  val equal : t -> t -> bool
  val hash : t -> int
end

module IntMap : Map.S with type key = int

module IdMon : MonadType with type 'a t = 'a

module MakeCollection
         (OT: sig
              include Set.OrderedType
              val print_in_fmt: Format.formatter -> t -> unit
            end)
       : Collection with type e = OT.t

module MakePATCollection(M: PHCons) : sig
  include Collection with type e = M.t
                      and type t = (M.t, unit, int, int, unit) General.Patricia.poly
  val id : t -> int
end
