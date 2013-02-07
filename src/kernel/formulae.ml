open Printf
open Format

module Term = struct

  type variables = string
  type fsymb = string

  type term = V of string | XV of string | C of string * (t list)
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
    | V(a), V(a') -> a = a' 
    | XV(a), XV(a') -> a = a' 
    | C(a,tl), C(a',tl') -> a = a' && equaltl (tl,tl')
    | V _, XV _ | V _, C(_, _) | XV _, V _ | XV _, C(_, _) |
      C(_, _), V _ | C(_, _), XV _  -> false 

  let rec hashtl = function
    | [] -> 1
    | t :: l -> t.id + 2 * hashtl l 

  let hash t1 =
    match t1.reveal with
    | V a -> 1 + 2 * Hashtbl.hash a
    | XV a -> 2 * Hashtbl.hash a
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
      (* print_endline(string_of_int(!atomid)); *)
	    incr atomid;
      H.add table f f;
      f

  let rec print_in_fmt fmt t =
    match t.reveal with
    | V a -> fprintf fmt "%s" a
    | XV a -> fprintf fmt "?%s" a
    | C(f, newtl) -> fprintf fmt "%s%a" f printtl_in_fmt newtl
  and printtl_in_fmt fmt tl =
    if tl <> [] then fprintf fmt "(%a)" printrtl_in_fmt tl
  and printrtl_in_fmt fmt tl =
    match tl with
    | [] -> ()
    | [t] -> print_in_fmt fmt t
    | t :: l -> fprintf fmt "%a,%a" print_in_fmt t printrtl_in_fmt tl

  let print_in_buf t buf = print_in_fmt (formatter_of_buffer buf) t

  let toString t =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt t;
    Buffer.contents buf

  let printtl tl =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" printtl_in_fmt tl;
    Buffer.contents buf
	
  let clear () = H.clear table
end 

module Atom = struct

  module Predicates = struct
    type t = {reveal : string; id : int}
    let reveal t = t.reveal
    let id t = t.id
    let table  = Hashtbl.create 5003 
    let predid = ref 0
    let build a =
	    try Hashtbl.find table a
	    with Not_found ->
        let f = {reveal = a; id = !predid} in
        (* print_endline(a^" "^string_of_int(!predid)); *)
	      incr predid;
        Hashtbl.add table a f;
        f
    let compare s s' = Pervasives.compare s.id s'.id
    let clear () = Hashtbl.clear table
  end

  type t = {reveal : bool * Predicates.t * Term.t list; id : int}

  let reveal t = t.reveal

  let id t = t.id

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

  let table = H.create 5003

  let attomid = ref 0

  let build a =
    let f = {reveal = a; id = !attomid} in
    try H.find table f
    with Not_found ->
      (* print_endline(string_of_int(!atomid)); *)
	    incr attomid;
      H.add table f f;
      f

  let bbuild (b,s,tl) = build (b, Predicates.build s, tl)

  let negation t = let (b, a, tl) = t.reveal in build (not b, a, tl)

  let print_in_fmt fmt t =
    match t.reveal with
    | (true, s, tl) ->
      fprintf fmt "{%s%a}" (Predicates.reveal s) Term.printtl_in_fmt tl
    | (false, s, tl) ->
      fprintf fmt "\\non {%s}%a" (Predicates.reveal s) Term.printtl_in_fmt tl

  let toString t =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt t;
    Buffer.contents buf

  let compare t t' = Pervasives.compare t.id t'.id

  let clear () =
    Predicates.clear();
    Term.clear()
end

type 'a form =
  | Lit of Atom.t
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a


(* Interface for an implementation of formulae *)

module type FormulaImplem = sig
  type t
  val reveal : t -> t form
  val build : t form -> t
end


(* Generic code providing standard functions about formulae *)

module PrintableFormula (F: FormulaImplem) = struct

  type t = F.t
      
  (* Displays a formula *)
  let rec print_in_fmt fmt f =
    match F.reveal f with
    | Lit l -> Atom.print_in_fmt fmt l
    | AndN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andN" f2
    | OrN(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\orN" f2
    | AndP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\andP" f2
    | OrP(f1, f2) -> print_bin_op_in_fmt fmt f1 "\\orP" f2
  and print_bin_op_in_fmt fmt f1 op f2 =
    fprintf fmt "(%a) %s (%a)" print_in_fmt f1 op print_in_fmt f2

  let toString f =
    let buf = Buffer.create 255 in
    fprintf (formatter_of_buffer buf) "%a%!" print_in_fmt f;
    Buffer.contents buf

  (* Negates a formula *)
  let rec negation f =
    let f1 = match F.reveal f with
    | Lit t -> Lit(Atom.negation t)
    | AndN(f1, f2) -> OrP(negation f1, negation f2)
    | OrN(f1, f2) -> AndP(negation f1, negation f2)
    | AndP(f1, f2) -> OrN(negation f1, negation f2)
    | OrP(f1, f2) -> AndN(negation f1, negation f2) in
    F.build f1

  let lit (b, f, tl) = F.build (Lit(Atom.bbuild (b, f, tl)))

  let andN (f1, f2) = F.build(AndN(f1, f2))

  let andP (f1, f2) = F.build(AndP(f1, f2))

  let orN (f1, f2)  = F.build(OrN(f1, f2))

  let orP (f1, f2)  = F.build(OrP(f1, f2))
end
