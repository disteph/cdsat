exception ParsingError of string

module type Type = sig
  type afterglance
  val name          : string
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string
  val parse         :
      ('sort Theories.forParser) -> (('sort,'t) Theories.interpretType) -> afterglance -> ('t option*bool option)
end
