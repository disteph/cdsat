(* This module contains the basic constructions of Patricia trees to
  represent maps and sets *)

include module type of Patricia_sig

type ('k,'v,'common,'branching,'ih) poly constraint 'ih=_*_

module MapH(I:MapArgH)
  : MapH with type keys    = I.t
          and type common  = I.common
          and type branching = I.branching
          and type values  = I.values
          and type infos   = I.infos
          and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly

module MapNH(I:MapArgNH)
  : MapNH with type keys    = I.t
           and type common  = I.common
           and type branching = I.branching
           and type values  = I.values
           and type infos   = I.infos
           and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly

module SetH(I:SetArgH)
  : SetH with type e       = I.t
          and type common  = I.common
          and type branching = I.branching
          and type infos   = I.infos
          and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly

module SetNH(I:SetArgNH)
  : SetNH with type e       = I.t
           and type common  = I.common
           and type branching = I.branching
           and type infos   = I.infos
           and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly
