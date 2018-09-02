(*****************)
(* Basic modules *)
(*****************)

open Format

include module type of Basic_sig

module IntSort : sig
  include PHCons
  val reveal : t -> int*Sorts.t
  val build  : int*Sorts.t -> t
  val isDefined  : t -> bool
end

module IntMap : Map.S with type key = int

module MakeCollection
         (OT: sig
              type t [@@deriving ord,show,hash]
            end) 
       : Collection with type e = OT.t

open General
open Patricia
open Patricia_tools
       
module MakePATCollection(M: PHCons) : sig
  include Collection with type e = M.t
                      and type t = (M.t, unit, int, int, EmptyInfo.infos*[`HCons]) poly
  val id : t -> int
end
