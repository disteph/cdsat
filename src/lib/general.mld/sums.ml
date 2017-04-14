(* Standard type constructs *)

open Format
       
type ('a,'b) sum = Case1 of 'a | Case2 of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No

let print_alm_in_fmt ?pp_yes ?pp_almost () fmt = function
  | Yes a -> (match pp_yes with Some f -> f fmt a | None -> fprintf fmt "Yes")
  | Almost a -> (match pp_almost with Some f -> f fmt a | None -> fprintf fmt "Almost")
  | No -> fprintf fmt "No" 

let pp_print_option ?pp_none print_in_fmt fmt = function
  | None ->
     begin
       match pp_none with
       | None -> fprintf fmt "None"
       | Some s -> s fmt ()
     end
  | Some a -> print_in_fmt fmt a
