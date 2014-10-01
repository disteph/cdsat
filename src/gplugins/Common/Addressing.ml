exception AddressingError of (bool*int) list*bool

type 'a address = (bool*int) list*((bool*int) option)*'a

let branch b (l,p,t) = match p with
  | None   -> (l,Some(b,0),t)
  | Some _ -> raise (AddressingError(l,true))

let branch_one (l,p) = match p with
  | None       -> raise (AddressingError(l,false))
  | Some (b,i) -> ( ((b,i)::l,None) , (l,Some(b,i+1)) )

let branch_two a = 
  let (a1,a2) = branch_one a in
  let (a2',_) = branch_one a2 in
  (a1,a2')

