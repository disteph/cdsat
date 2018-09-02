type (_,_,_) t =
  | Eq : ('a,'a,[< `Eq | `IsEq | `Ord]) t
  | Neq: (_,_,[`IsEq]) t
  | Gt : (_,_,[`Ord]) t
  | Lt : (_,_,[`Ord]) t

let pp fmt (type a b c) : (a,b,c) t -> unit = function
  | Eq  -> Format.fprintf fmt "Eq"
  | Neq -> Format.fprintf fmt "Neq"
  | Gt  -> Format.fprintf fmt "Gt"
  | Lt  -> Format.fprintf fmt "Lt"

let show e = Print.stringOf pp e

type ('a,'b) eq   = ('a,'b,[`Eq]) t
type ('a,'b) iseq = ('a,'b,[`IsEq]) t
type ('a,'b) ord  = ('a,'b,[`Ord]) t

exception NotEq

let iseq (type a b) : (a,b,[< `Eq | `IsEq | `Ord]) t -> (a,b) iseq = function
  | Eq -> Eq
  | _ -> Neq

let eq (type a b) : (a,b,[< `Eq | `IsEq | `Ord]) t -> (a,b) eq = function
  | Eq -> Eq
  | _ -> raise NotEq
