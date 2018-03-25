(* Standard type constructs *)

open Format

let pp_print_option ?pp_none print_in_fmt fmt = function
  | Some a -> print_in_fmt fmt a
  | None ->
     match pp_none with
     | None -> fprintf fmt "None"
     | Some s -> s fmt ()

let map f = function
  | None -> None
  | Some a -> Some(f a)

let is_none = function None -> true | Some _ -> false
let is_some = function None -> false | Some _ -> true

type some = private S
type none = private N

type (_,_) gadt =
  | Some: 'a -> ('a,some) gadt
  | None: ('a,none) gadt

