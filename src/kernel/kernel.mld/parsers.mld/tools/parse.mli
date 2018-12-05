open Parser
open Top

val latexescaped : char -> string
       
val sort   : decsorts:string list -> sort -> Sorts.t

val symbol : decsorts:string list
  -> (Symbols.t -> Terms.TermB.t list -> Terms.TermB.t)
  -> ident -> Terms.TermB.t list -> Terms.TermB.t
