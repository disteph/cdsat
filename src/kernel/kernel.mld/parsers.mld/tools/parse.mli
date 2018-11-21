open Parser

val latexescaped : char -> string
       
val sort      : decsorts:string list -> sort -> Top.Sorts.t

val multiary  : Top.Symbols.t -> 'a Multiary.multiary option
    
val symbol    : decsorts:string list -> string -> Top.Sorts.t list -> Top.Symbols.t
