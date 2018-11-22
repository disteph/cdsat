(****************************************************************)
(**        Attempting to implement QuickXplain in OCaml        **)
(****************************************************************)
(**.mli file**)


(*Generic types for set elements and sets of elements*)

type element
   
type set
   
val empty : set
  
val is_empty : set -> bool
  
val union : set -> set -> set

val cardinal : set -> int

  
(*Notion of consistency of a set of elements.*)
  
val isConsistent : set -> bool

  
(*Our divide-and-conquer tactic: tells us where to split.*)

val split : int -> int
  

(*halves returns two halves of c, separated according to our divide-and-conquer tactic split.*)

val halves : (set -> set -> bool) -> set -> set*set
  

(*quickXplain algorithm*)

val quickXplain : set -> set -> (set -> set -> bool) -> set option
