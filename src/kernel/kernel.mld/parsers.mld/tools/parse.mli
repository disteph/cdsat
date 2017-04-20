open Parser

val latexescaped : char -> string
       
val sort      : decsorts:string list -> sort -> Top.Sorts.t

val multiary  : Top.Symbols.t -> ((('a list->'a list) -> 'a list -> 'a) option)
val symbol    : decsorts:string list -> string -> Top.Symbols.t list
