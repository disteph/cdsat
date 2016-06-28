(* Standard type constructs *)

type 'a t = 'a list * 'a list

let empty = [],[]
                 
let push e (l,r) = e::l, r
                     
let pop = function
  | [],[]  -> None
  | l,e::r -> Some(e,(l,r))
  | l,[] -> let l' = List.rev l in
            Some(List.hd l',([],List.tl l'))
