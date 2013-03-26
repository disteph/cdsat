exception ParsingError of string

module type Type = sig
  type afterglance
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string
  val parse         :
      ('sort Theories.forParser) -> (('sort,'t) Theories_tools.interpretType) -> afterglance -> ('t option*bool option)
end
