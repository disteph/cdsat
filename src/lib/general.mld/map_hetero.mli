include module type of Map_hetero_sig

type ('k,'v,'common,'branching,'hcons) poly

module MakeH(I:ArgH)
  : S_H with type 'a keys   = 'a I.t
         and type 'a values = 'a I.values
         and type common    = I.common
         and type branching = I.branching

module MakeNH(I:ArgNH)
  : S_NH with type 'a keys   = 'a I.t
          and type 'a values = 'a I.values
          and type common    = I.common
          and type branching = I.branching
