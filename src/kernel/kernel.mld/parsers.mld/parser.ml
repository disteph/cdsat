open Top.Sorts
       
exception ParsingError of string

type sortType = Sort of string*(sortType list)

(* Type of functions used to type-check and interpret an untyped AST.
- For symbols declared in the signature (sigsymb), we have to understand
which symbol is meant by the string appearing in the input file.
- For symbols declared in the input file (decsymb), the file
should specify the full arity, with the sorts of arguments.
- For quantifiers, the string list is for binding several variables at the same time,
the strings being their sorts. *)

module type InterpretType = sig
  type t
  val sigsymb : string                             -> t list -> t
  val decsymb : string->(sortType*(sortType list)) -> t list -> t
  val boundsymb : int -> sortType                            -> t
  val quantif : bool -> sortType list              -> t      -> t
end

module type Type = sig
  type afterglance
  val name          : string
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string list option
  val parse         :
    afterglance -> (string list -> (module InterpretType with type t = 't)) -> ('t option*bool option)
end
