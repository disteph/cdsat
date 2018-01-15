(*******************************)
(* Standard multiary functions *)
(*******************************)

exception MultiaryError of string

val singleton : 'a list -> 'a
  
type 'a multiary = (Top.Symbols.t -> 'a list -> 'a list) -> Top.Symbols.t -> 'a list -> 'a

(* r_assoc is for right-associative symbols *)
val r_assoc : 'a multiary

(* l_assoc is for left-associative symbols *)
val l_assoc : 'a multiary

(* pairwise is for parwise symbols such as Eq and Neq *)
val pairwise : 'a multiary
