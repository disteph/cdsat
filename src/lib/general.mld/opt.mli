(* Standard type constructs *)

open Format

val pp_print_option :
  ?pp_none:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a option
  -> unit

val map : ('a -> 'b) -> 'a option -> 'b option
       
type some = private S
type none = private N

type (_,_) gadt = 
  | Some: 'a -> ('a,some) gadt
  | None: ('a,none) gadt


