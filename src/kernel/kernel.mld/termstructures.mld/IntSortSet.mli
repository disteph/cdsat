open General
open Top.Basic

include Patricia.Set with type e = IntSort.t
                      and type ('v,'i) param = (IntSort.t,'v,int,int,'i) Patricia.poly
val pp : t Format.printer
