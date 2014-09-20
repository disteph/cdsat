open Format

module type TermType = sig
  type fsymb
  type leaf
  type t
  type term = V of leaf | C of fsymb * (t list)
  val reveal : t -> term
  val bV : leaf -> t
  val bC : fsymb -> t list -> t
  val subst : (leaf -> leaf) -> t -> t
  val id     : t -> int
  val print_in_fmt: Format.formatter -> t -> unit
  val clear : unit -> unit
end

module TermMake(Leaf: Kernel.Interfaces_I.PHCons) = struct

  type fsymb = string
  type leaf  = Leaf.t

  (* A term is either a variable or a function symbol applied to
  arguments *)

  type term = V of leaf | C of fsymb * (t list)
  and t = {reveal : term; id : int}

  let reveal f = f.reveal
  let id f     = f.id

  let rec equaltl = function
    | ([], []) -> true
    | ((t :: l), (t' :: l')) -> t == t' && equaltl (l, l')
    | ([], (_ :: _)) | ((_ :: _), []) -> false 

  let equal t1 t2 =
    match t1.reveal, t2.reveal with
    | V(a), V(a')        -> Leaf.compare a a' == 0
    | C(a,tl), C(a',tl') -> a = a' && equaltl(tl,tl')
    | V _, C(_, _) 
    | C(_, _), V _  -> false 

  let rec hashtl = function
    | [] -> 1
    | t :: l -> t.id + 2 * hashtl l 

  let hash t1 =
    match t1.reveal with
    | V a    -> 1 + 2 * Leaf.id a
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

  let bV i   = build(V i)
  let bC f l = build(C(f,l))
        
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
      else fprintf fmt "%a,%a" print_in_fmt t printrtl_in_fmt l
	
  let clear () = atomid := 0; H.clear table

  let rec subst update t = match reveal t with
    | V i    -> bV (update i)
    | C(f,l) -> bC f (List.map (subst update) l)

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

  module Term = TermMake(Leaf)

  type t = {reveal : bool * Predicates.t * Term.t list; id : int}

  let reveal t = t.reveal
  let id t     = t.id

  let equal t t'= 
    let (b, a, tl) = t.reveal in
    let (b', a', tl') = t'.reveal in
	  b = b' && Predicates.compare a a' == 0 && Term.equaltl (tl, tl')

  let hash t =
    let (b, a, tl) = t.reveal in
    (if b then 0 else 1) + 2 * Hashtbl.hash a + 3 * Term.hashtl tl

  module H = Hashtbl.Make(struct
    type t1 = t
    type t = t1
    let equal = equal
    let hash = hash
  end)

  let print_in_fmt fmt t =
    match t.reveal with
    | (true, s, tl) ->
      if tl<>[] then fprintf fmt "{%s%a}" (Predicates.reveal s) Term.printtl_in_fmt tl
      else fprintf fmt "{%s}" (Predicates.reveal s)
    | (false, s, tl) ->
      if tl<>[] then fprintf fmt "\\non {%s}%a" (Predicates.reveal s) Term.printtl_in_fmt tl
      else fprintf fmt "\\non {%s}" (Predicates.reveal s)

  let table = H.create 5003

  let attomid = ref 0

  let build a =
    let f = {reveal = a; id = !attomid} in
    try H.find table f
    with Not_found ->
      incr attomid;
      H.add table f f;
      f

  let bbuild (b,s,tl) = build (b, Predicates.build s, tl)

  let negation t = let (b, a, tl) = t.reveal in build (not b, a, tl)

  let compare t t' = Pervasives.compare t.id t'.id

  let clear () =
    attomid := 0;
    Predicates.clear();
    Term.clear();
    H.clear table
end
