(* Standard type constructs *)

open Format

val pp_print_option :
  ?pp_none:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a option
  -> unit

type some = private S
type none = private N

type (_,_) t = 
  | Some: 'a -> ('a,some) t
  | None: ('a,none) t


