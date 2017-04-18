(*******************************)
(* Standard multiary functions *)
(*******************************)

exception MultiaryError of string

val singleton : 'a list -> 'a

(* r_assoc is for right-associative symbols *)
val r_assoc : ('a list->'a list) -> 'a list -> 'a

(* l_assoc is for left-associative symbols *)
val l_assoc : ('a list->'a list) -> 'a list -> 'a

(* pairwise is for parwise symbols such as Eq and Neq *)
val pairwise : 'a -> ('a list -> 'a) -> ('a list->'a list) -> 'a list -> 'a
