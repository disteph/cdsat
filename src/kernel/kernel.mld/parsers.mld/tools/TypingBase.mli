(*******************************)
(* Standard multiary functions *)
(*******************************)

open Top.Terms
       
exception TypingError of string

type multiary =
  (Top.Symbols.t -> TermB.t list -> TermB.t) ->
  Top.Symbols.t -> TermB.t list -> TermB.t

(* returns unique element of list *)
val singleton : TermB.t list -> TermB.t
  
(* exact just accepts a symbol with arguments exactly matching the arity *)
val exact : multiary

(* r_assoc is for right-associative symbols *)
val r_assoc : multiary

(* l_assoc is for left-associative symbols *)
val l_assoc : multiary

(* pairwise is for parwise symbols such as Eq and Neq *)
val pairwise : multiary

(* Combining different possibilities for parsing *)
val trythese : (TermB.t list -> TermB.t) list -> TermB.t list -> TermB.t
