include module type of Map_hetero_sig

type ('k,'v,'common,'branching,'hcons) poly

module MapH(I:MapArgH)
  : MapH with type 'a keys   = 'a I.t
          and type 'a values = 'a I.values
          and type common    = I.common
          and type branching = I.branching

module MapNH(I:MapArgNH)
  : MapNH with type 'a keys   = 'a I.t
           and type 'a values = 'a I.values
           and type common    = I.common
           and type branching = I.branching
