exception ParsingError of string

module type Type = sig
  type afterglance
  val name          : string
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string list option
  val parse         :
    afterglance -> (module Kernel.Typing.InterpretType with type t = 't) -> ('t option*bool option)
end

let latexescaped = function
  | '%' | '{' | '}' as c -> "\\"^Char.escaped c
  | c -> Char.escaped c
