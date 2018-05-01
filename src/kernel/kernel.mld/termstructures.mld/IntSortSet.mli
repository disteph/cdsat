open General
open Top.Basic

include Patricia.PatSet.S with type e = IntSort.t
                           and type ('v,'i) param = (IntSort.t,'v,int,int,'i) Patricia.poly
val pp : Format.formatter -> t -> unit
