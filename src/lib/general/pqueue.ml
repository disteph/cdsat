(* Standard type constructs *)

type 'a t = ('a list * 'a list) ref

let empty () = ref([],[])
                 
let push e queue = let l,r = !queue in ref(e::l, r)

let pop queue = match !queue with
  | l,e::r -> Some(e,ref (l,r))
  | l,[] -> match List.rev l with
            | [] -> None
            | e::r as l' -> queue := [], l'; Some(e, ref ([],r))
