open Kernel.Theories.Bitvectors
  
type _ t [@@deriving show]
          
val init : int -> 'a t
val pick : 'a t -> MyTheory.V.t
val mem : MyTheory.V.t -> 'a t -> bool

type 'a update =
  | Range of 'a t
  | Singleton of MyTheory.V.t
  | Empty of 'a list

val update : Signal.t -> 'a -> 'a t -> 'a update
