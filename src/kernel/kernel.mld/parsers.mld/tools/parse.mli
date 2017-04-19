open Parser

val latexescaped : char -> string
       
val sort      : string list -> sortType -> Top.Sorts.t

val multiary  : Top.Symbols.t -> ((('a list->'a list) -> 'a list -> 'a) option)
val symbol    : string list -> string -> Top.Symbols.t list
