(* Standard type constructs *)

type ('a,'b) sum =
  | Case1 of 'a
  | Case2 of 'b
               [@@deriving eq, ord, show, hash]
               

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No
[@@deriving eq, ord, show, hash]


open Format

val print_alm_in_fmt :
  ?pp_yes:(formatter -> 'a -> unit)
  -> ?pp_almost:(formatter -> 'b -> unit)
  -> unit
  -> formatter
  -> ('a,'b) almost
  -> unit
