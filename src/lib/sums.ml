(* Standard type constructs *)

type ('a,'b) sum = A of 'a | F of 'b

type ('a,'b) almost =
  | Yes of 'a
  | Almost of 'b
  | No
