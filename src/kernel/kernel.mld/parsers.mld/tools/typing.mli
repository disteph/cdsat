(***************************************************)
(* Specifications and tools for parsing and typing *)
(***************************************************)

open Top
    
module ForParsing : Parser.ForParsing with type t = Terms.TermB.t

val forParser :
  (module Parser.ForParsing with type t = Terms.TermB.t)
  -> decsorts:string list
  -> (module Parser.InterpretType with type t = Terms.TermB.t)
