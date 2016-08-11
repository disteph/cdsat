(* Standard type constructs *)

type ('a,'b) sum = Case1 of 'a | Case2 of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No

let pp_print_option ?pp_none print_in_fmt fmt = function
  | None ->
     begin
       match pp_none with
       | None -> Format.fprintf fmt "None"
       | Some s -> s fmt ()
     end
  | Some a -> print_in_fmt fmt a
