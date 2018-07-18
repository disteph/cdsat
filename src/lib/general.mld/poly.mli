type (_,_,_) t =
  | Eq : ('a,'a,[< `Eq | `IsEq | `Ord]) t
  | Neq: (_,_,[`IsEq]) t
  | Gt : (_,_,[`Ord]) t
  | Lt : (_,_,[`Ord]) t

type ('a,'b) eq   = ('a,'b,[`Eq]) t
type ('a,'b) iseq = ('a,'b,[`IsEq]) t
type ('a,'b) ord  = ('a,'b,[`Ord]) t

val iseq : ('a,'b,[< `Eq | `IsEq | `Ord]) t -> ('a,'b) iseq

exception NotEq  
val eq   : ('a,'b,[< `Eq | `IsEq | `Ord]) t -> ('a,'b) eq
