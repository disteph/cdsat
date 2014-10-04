type nodeType = AndNode | OrNode
    
type 'a addressing = {
  path    : (nodeType*int) list;
  current : (nodeType*int) option; 
  data : 'a
}

exception AddressingError of string

let el_wrap a = function
  | [] -> a
  | _  ->
    raise (AddressingError("Trying to /\\-branch on address with current node"))

let ad_init i = el_wrap {path = []; current = None; data = i}

let ad_up ad newdata = {path = ad.path; current = ad.current; data = newdata}

let branch b ad = match ad.current with
  | None   -> {path = ad.path; current = Some(b,0); data = ad.data}
  | Some _ -> 
    raise (AddressingError("Trying to add current node to address that already has one"))

let branch_one ad = match ad.current with
  | Some(OrNode,i) ->
    let rec aux = function
      | []   -> (OrNode,i)::ad.path
      | b::l -> (AndNode,if b then 0 else 1)::(aux l)
    in
    ( (fun l -> {path = aux l; current = None; data = ad.data}),
      el_wrap {path = ad.path; current = Some(OrNode,i+1); data = ad.data} )
  | _      -> 
    raise (AddressingError("Trying to \\/-branch on address that has no current node"))

let branch_two ad = 
  let (a1,a2) = branch_one ad in
  let (a2',_) = branch_one (a2 []) in
  (a1,a2')

