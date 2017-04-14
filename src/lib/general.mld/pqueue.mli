type 'a t
val empty : unit -> 'a t
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> ('a*('a t)) option
