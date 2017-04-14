(* Standard type constructs *)

type ('a,'b) sum = Case1 of 'a | Case2 of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No

open Format

val print_alm_in_fmt :
  ?pp_yes:(formatter -> 'a -> unit)
  -> ?pp_almost:(formatter -> 'b -> unit)
  -> unit
  -> formatter
  -> ('a,'b) almost
  -> unit

val pp_print_option :
  ?pp_none:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a option
  -> unit
