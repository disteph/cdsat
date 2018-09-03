(***************************************************)
(* Specifications and tools for parsing and typing *)
(***************************************************)

open Top
    
exception TypingError of string

module ForParsing : Parser.ForParsing with type t = Terms.TermB.t

val forParser :
  (module Parser.ForParsing with type t = 'a)
  -> decsorts:string list
  -> (module Parser.InterpretType with type t = Sorts.t -> 'a)
