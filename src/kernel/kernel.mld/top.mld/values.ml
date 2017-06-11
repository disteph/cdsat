(**********)
(* Values *)
(**********)

open Format

type _ value =
  | NonBoolean : 'value -> 'value value
  | Boolean    : bool   -> _ value

(* Printing values *)

let pp (type a) valueprint fmt: a value -> unit = function
  | NonBoolean v -> fprintf fmt "%a" valueprint v
  | Boolean b    -> fprintf fmt "%b" b

