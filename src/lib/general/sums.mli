(* Standard type constructs *)

type ('a,'b) sum = A of 'a | F of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No

open Format

val pp_print_option : ?pp_none:(formatter -> unit -> unit) ->
                      (formatter -> 'a -> unit)
                      -> formatter
                      -> 'a option
                      -> unit
