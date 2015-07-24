type nodeType = AndNode | OrNode
    
type 'a addressing = {
  path    : (nodeType*int) list;
  current : (nodeType*int) option; 
  data : 'a
}

exception AddressingError of string

let print_in_fmt_ad fmt ad =
  let rec aux fmt = function
    | [] -> ()
    | (AndNode,i)::l -> Format.fprintf fmt "%a∧%i" aux l i
    | (OrNode,i)::l  -> Format.fprintf fmt "%a∨%i" aux l i
  in
  Format.fprintf fmt "%a%t" aux ad.path
  (function fmt -> match ad.current with
  | None -> ()
  | Some(AndNode,i) -> Format.fprintf fmt "[∧%i]" i
  | Some(OrNode,i)  -> Format.fprintf fmt "[∨%i]" i)

let ad_up ad newdata = {path = ad.path; current = ad.current; data = newdata}

let add2path pa i l =
  let rec aux acc = function
    | []    -> acc
    | b::l' -> aux ((AndNode,if b then 0 else 1)::acc) l'
  in
  {path = aux pa l; current = None; data = i}

let ad_init i = add2path [] i

let branch b ad = match ad.current with
  | None   -> {path = ad.path; current = Some(b,0); data = ad.data}
  | Some _ -> 
    raise (AddressingError "Trying to add current node to address that already has one")

let el_wrap a = function
  | [] -> a
  | _  ->
    raise (AddressingError "Trying to ∧-branch on address with current node")

let branch_one ad = match ad.current with
  | Some(OrNode,i) ->
    ( add2path ((OrNode,i)::ad.path) ad.data,
      el_wrap {path = ad.path; current = Some(OrNode,i+1); data = ad.data} )
  | _      ->
    raise (AddressingError "Trying to ∨-branch on address that has no current node")

let branch_two ad = 
  let (a1,a2) = branch_one ad in
  let (a2',_) = branch_one (a2 []) in
  (a1,a2')

