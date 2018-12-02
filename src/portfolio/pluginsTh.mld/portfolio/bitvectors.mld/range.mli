open Kernel.Theories.Bitvectors
  
type _ t [@@deriving show]
          
val init : int -> 'a t
val pick : 'a t -> V.t
val mem : V.t -> 'a t -> bool

type 'a update =
  | Range of 'a t
  | Singleton of V.t
  | Empty of ('a * BDD.t) list

val update : Signal.t -> 'a -> 'a t -> 'a update

val find_explanation : 'a update -> ('a * BDD.t) list option
