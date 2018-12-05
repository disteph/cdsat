open Kernel
open Top
open Theories.Bitvectors
  
type _ t [@@deriving show]
          
val init : int -> 'a t
val pick : 'a t -> Bv_value.t
val mem : Bv_value.t -> 'a t -> bool

type 'a update =
  | Range of 'a t
  | Singleton of Bv_value.t
  | Empty of 'a list

val update : Signal.t -> 'a -> 'a t -> 'a update
