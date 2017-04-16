(* Standard type constructs *)

open Format

let pp_print_option ?pp_none print_in_fmt fmt = function
  | None ->
     begin
       match pp_none with
       | None -> fprintf fmt "None"
       | Some s -> s fmt ()
     end
  | Some a -> print_in_fmt fmt a

type some = private S
type none = private N

type (_,_) t = 
  | Some: 'a -> ('a,some) t
  | None: ('a,none) t


