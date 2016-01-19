(* This module contains the basic constructions of Patricia trees to
  represent maps and sets *)

open Patricia_interfaces

type ('keys,'values,'common,'branching,'infos) poly

module Poly(I:Intern): sig

  open I

end

module PATMap: sig

  module type S = PATMapType

  module Make
    (D:MapDestType)
    (I:Intern with type keys=D.keys)
    : S with type keys   = D.keys
        and  type values = D.values
        and  type infos  = D.infos
        and  type common = I.common
        and  type branching = I.branching
        and  type ('v,'i) param = (D.keys,'v,I.common,I.branching,'i) poly
        and  type t = (D.keys,D.values,I.common,I.branching,D.infos) poly
end

module PATSet: sig

  module type S = PATSetType

  module Make
    (D:SetDestType)
    (I:Intern with type keys=D.keys)
    : S with type e      = D.keys
        and  type infos  = D.infos
        and  type common = I.common
        and  type branching = I.branching
        and  type t = (D.keys,unit,I.common,I.branching,D.infos) poly
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
