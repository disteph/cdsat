(**********)
(* Values *)
(**********)

type 'v t =
  | NonBoolean of 'v
  | Boolean    of bool
                    [@@deriving eq, ord, show, hash]

val boolassign : ('term*bool) -> 'term*(_ t)
val bassign : ?b:bool -> 'term -> 'term*(_ t)
