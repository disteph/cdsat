(* This module contains the basic constructions of Patricia trees to
   represent maps and sets *)

open Patricia_sig

module type Key = Key

type nonrec ('keys,'values,'infos) info_build_type
  = ('keys,'values,'infos) info_build_type
  = { empty_info  : 'infos;
      leaf_info   : 'keys -> 'values -> 'infos;
      branch_info : 'infos -> 'infos -> 'infos }

type ('k,'v,'common,'branching,'ih) poly constraint 'ih=_*_

module Map : sig

  module type S = sig
    type keys
    type common
    type branching
    include Map with type keys   := keys
                 and type common := common
                 and type branching := branching
                 and type ('v,'ih)param = (keys,'v,common,branching,'ih) poly
  end

  module type ArgH = MapArgH
  module type S_H  = sig
    include S with type hcons = [`HCons]
    val equal    : t Equal.t
    val hash_fold_t : t Hash.folder
    val hash     : t Hash.t
    val compare  : t Compare.t
    val id       : t -> int
    val clear    : unit -> unit
  end
  module MakeH(I:ArgH)
    : S_H with type keys    = I.t
           and type common  = I.common
           and type branching = I.branching
           and type values  = I.values
           and type infos   = I.infos

  module type ArgNH = MapArgNH
  module type S_NH = S with type hcons = [`NoHCons]
  module MakeNH(I:ArgNH)
    : S_NH with type keys    = I.t
            and type common  = I.common
            and type branching = I.branching
            and type values  = I.values
            and type infos   = I.infos
end

module Set : sig

  module type S = sig
    type e
    type common
    type branching
    include Set with type e         := e
                 and type common    := common
                 and type branching := branching
                 and type ('v,'ih)param = (e,'v,common,branching,'ih) poly
  end

  module type ArgH = SetArgH
  module type S_H  = sig
    include S with type hcons = [`HCons]
    val equal    : t Equal.t
    val hash_fold_t : t Hash.folder
    val hash     : t Hash.t
    val compare  : t Compare.t
    val id       : t -> int
    val clear    : unit -> unit
  end
  module MakeH(I:ArgH)
    : S_H with type e       = I.t
           and type common  = I.common
           and type branching = I.branching
           and type infos   = I.infos
           and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly

  module type ArgNH = SetArgNH
  module type S_NH = S with type hcons = [`NoHCons]
  module MakeNH(I:ArgNH)
    : S_NH with type e       = I.t
            and type common  = I.common
            and type branching = I.branching
            and type infos   = I.infos
            and type ('v,'ih)param = (I.t,'v,I.common,I.branching,'ih) poly
end
