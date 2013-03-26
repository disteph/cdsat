module type Type = sig
  type afterglance
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string
  val parse         :
      ('sort Theories.forParser) -> (('sort,'t) Parsing_tools.interpretType) -> afterglance -> ('t option*bool option)
end
