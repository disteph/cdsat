(* This is the register of all parsers in Psyche *)

type t =
  | SMTLib2
  | DIMACS
[@@deriving show, enumerate]

val get : t -> (module Parser.Type)

exception NotFound of string

val parse_name : string -> t

val parse : t
            -> string
            -> (string list option)
               * (Top.Terms.TermB.t list)
               * (bool option)

