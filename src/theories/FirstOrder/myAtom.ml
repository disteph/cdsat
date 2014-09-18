open Format

module Term(Leaf: Kernel.Interfaces_I.PHCons) = struct

  type fsymb = string

  type term = V of Leaf.t | C of fsymb * (t list)
  and t = {reveal : term; id : int}
	(* A term is either a variable or a function symbol applied to arguments *)

  let reveal f = f.reveal

  let id f = f.id

  let rec equaltl = function
    | ([], []) -> true
    | ((t :: l), (t' :: l')) -> t == t' && equaltl (l, l')
    | ([], (_ :: _)) | ((_ :: _), []) -> false 

  let equal t1 t2 =
    match t1.reveal, t2.reveal with
    | V(a), V(a') -> Leaf.compare a a' == 0
    | C(a,tl), C(a',tl') -> a = a' && equaltl(tl,tl')
    | V _, C(_, _) |  C(_, _), V _  -> false 

  let rec hashtl = function
    | [] -> 1
    | t :: l -> t.id + 2 * hashtl l 

  let hash t1 =
    match t1.reveal with
    | V a -> 1 + 2 * Leaf.id a
    | C(a,l) -> 3 * Hashtbl.hash a + 7 * hashtl l

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let hash = hash
    let equal = equal
  end)

  let table = H.create 5003

  let atomid = ref 0

  let build a =
    let f = {reveal = a; id = !atomid} in
    try H.find table f
    with Not_found ->
      incr atomid;
      H.add table f f;
      f
        
  let rec print_in_fmt fmt t =
    match t.reveal with
    | V a -> fprintf fmt "%a" Leaf.print_in_fmt a
    | C(f, newtl) -> fprintf fmt "%s%a" f printtl_in_fmt newtl
  and printtl_in_fmt fmt tl =
    if tl <> [] then fprintf fmt "(%a)" printrtl_in_fmt tl
  and printrtl_in_fmt fmt tl =
    match tl with
    | [] -> ()
    | t :: l ->
      if l = [] then print_in_fmt fmt t
      else fprintf fmt "%a,%a" print_in_fmt t printrtl_in_fmt tl

  let print_in_buf t buf = print_in_fmt (formatter_of_buffer buf) t

  let toString t =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt t;
    Buffer.contents buf

  let printtl tl =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" printtl_in_fmt tl;
    Buffer.contents buf
	
  let clear () = atomid := 0; H.clear table
end 

module Predicates = struct
  type psymb = string
  type t = {reveal : psymb; id : int}
  let reveal t = t.reveal
  let id t = t.id
  let table  = Hashtbl.create 5003 
  let predid = ref 0
  let build a =
    let f = {reveal = a; id = !predid} in
    try Hashtbl.find table a
    with Not_found ->
      incr predid;
      Hashtbl.add table a f;
      f
  let compare s s' = Pervasives.compare s.id s'.id
  let clear () = predid := 0;  Hashtbl.clear table
end

module Atom(Leaf : Kernel.Interfaces_I.PHCons) = struct

  module MyTerm = Term(Leaf)

  type t = {reveal : bool * Predicates.t * MyTerm.t list; id : int}

  let reveal t = t.reveal

  let id t = t.id

  let equal t t'= 
    let (b, a, tl) = t.reveal in
    let (b', a', tl') = t'.reveal in
	  b = b' && Predicates.compare a a' == 0 && MyTerm.equaltl (tl, tl')

  let hash t =
    let (b, a, tl) = t.reveal in
    (if b then 0 else 1) + 2 * Hashtbl.hash a + 3 * MyTerm.hashtl tl

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let equal = equal
    let hash = hash
  end)

  let print_in_fmt fmt t =
    match t.reveal with
    | (true, s, tl) ->
      if tl<>[] then fprintf fmt "{%s%a}" (Predicates.reveal s) MyTerm.printtl_in_fmt tl
      else fprintf fmt "{%s}" (Predicates.reveal s)
    | (false, s, tl) ->
      if tl<>[] then fprintf fmt "\\non {%s}%a" (Predicates.reveal s) MyTerm.printtl_in_fmt tl
      else fprintf fmt "\\non {%s}" (Predicates.reveal s)

  let toString t =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt t;
    (* let (b,p,tl)=t.reveal in *)
    (* (if b then "+" else "-")^"("^Predicates.reveal p^string_of_int (Predicates.id p)^")"^(string_of_int t.id) *)
    Buffer.contents buf
    (* ^"-"^(string_of_int t.id) *)

  let table = H.create 5003

  let attomid = ref 0

  let build a =
    let f = {reveal = a; id = !attomid} in
    try H.find table f
    with Not_found ->
      (* print_endline("HashConsing "^toString f); *)
      incr attomid;
      H.add table f f;
      f

  let bbuild (b,s,tl) = build (b, Predicates.build s, tl)

  let negation t = let (b, a, tl) = t.reveal in build (not b, a, tl)


  let compare t t' = Pervasives.compare t.id t'.id

  let clear () =
    attomid := 0;
    Predicates.clear();
    MyTerm.clear();
    H.clear table
end
