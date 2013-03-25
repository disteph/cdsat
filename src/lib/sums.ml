(* Standard type constructs *)

type ('a,'b) sum = A of 'a | F of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No

(* Remembers time of start *)

let start_time    = ref (Sys.time())
let last_display  = ref (Sys.time())

let print_time report =
  let b=Sys.time() in
  let c=b-. !last_display  in
    if c>float_of_int Flags.every.(8) then
      (last_display:=b;
       print_endline(string_of_int (int_of_float(b-. !start_time))
		     ^" seconds");
       report 1)
