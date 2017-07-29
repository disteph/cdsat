(* This module contains the basic constructions of Patricia trees to
  represent maps and sets *)

open Patricia_interfaces

type ('keys,'values,'common,'branching,'infos) poly

module PatMap: sig

  module type S = PatMap

  module Make
    (K:MapArg)
    (I:Intern with type keys=K.t)
    : S with type keys   = K.t
        and  type values = K.values
        and  type infos  = K.infos
        and  type common = I.common
        and  type branching = I.branching
        and  type ('v,'i) param = (K.t,'v,I.common,I.branching,'i) poly
end

module PatSet: sig

  module type S = PatSet

  module Make
    (E:SetArg)
    (I:Intern with type keys=E.t)
    : S with type e      = E.t
        and  type infos  = E.infos
        and  type common = I.common
        and  type branching = I.branching
        and  type ('v,'i) param = (E.t,'v,I.common,I.branching,'i) poly
end
