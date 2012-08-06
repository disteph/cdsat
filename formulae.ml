module Term = 
  (struct

     type variables = string
     type fsymb = string

     module TermPrimitive = struct
       type 'a term = V of string | XV of string | C of string*('a list)
       type t = {reveal:t term;id:int}
	   (* A term is either a variable or a function symbol applied to arguments *)
       let reveal f = f.reveal
       let id f = f.id
       let rec equaltl = function
	 | [],[]           -> true
	 | (t::l),(t'::l') -> t==t'&& equaltl(l,l')
	 | _                 -> false 
       let equal t1 t2 = match t1.reveal,t2.reveal with
	 | V(a), V(a')       -> a=a' 
	 | XV(a), XV(a')     -> a=a' 
	 | C(a,tl), C(a',tl')-> a=a' && equaltl(tl,tl')
	 | _                 -> false 
       let rec hashtl = function
	 | []   -> 1
	 | t::l -> t.id+2*hashtl l 
       let hash t1 = match t1.reveal with
	 | V(a)   -> 1+2*(Hashtbl.hash a)
	 | XV(a)  -> 2*(Hashtbl.hash a)
	 | C(a,l) -> 3*(Hashtbl.hash a)+7*(hashtl l)
     end

     include TermPrimitive
     module H = Hashtbl.Make(TermPrimitive)

     let table = H.create 5003
     let atomid =ref 0
     let build a =
       let f = {reveal =  a; id = !atomid } in
	 try H.find table f
	 with Not_found ->  (* print_endline(string_of_int(!atomid)); *)
	   incr atomid; H.add table f f; f

     let rec toString t = match t.reveal with
	 V(a) -> a
       | XV(a) -> "?"^a
       | C(f, newtl) -> f^printtl(newtl)
     and printrtl = function
	 [] -> ""
       | t::[] -> toString(t)
       | t::l -> toString(t)^", "^printtl(l)
     and printtl = function
	 [] -> ""
       | tl ->"("^printrtl(tl)^")"
	   
   end:sig
     type variables
     type fsymb
     type 'a term = V of variables | XV of variables | C of fsymb*('a list)
     type t
     val reveal: t -> t term
     val build: t term -> t
     val id: t-> int       
     val toString: t-> string
     val printtl: t list -> string
     val equal: t->t->bool
     val equaltl: ((t list)*(t list))->bool
     val hash: t -> int
     val hashtl: t list ->int
   end) 


module Atom =
  (struct
     module Predicates = struct
       type t = string
       let compare s s' = Pervasives.compare s s'
     end
     module AtomPrimitive = struct
       type t = {reveal:bool*Predicates.t*(Term.t list);id:int}
       let reveal t = t.reveal
       let id t = t.id
       let equal t t'= 
	 let (b,a,tl) = t.reveal in
	 let (b',a',tl') = t'.reveal in
	   b=b'&&a=a'&&Term.equaltl(tl,tl')
       let hash t = let (b,a,tl)= t.reveal in (if b then 0 else 1)+2*(Hashtbl.hash a)+3*(Term.hashtl tl)
     end
     include AtomPrimitive
     module H = Hashtbl.Make(AtomPrimitive)
     let table = H.create 5003
     let atomid =ref 0
     let build a =
       let f = {reveal =  a; id = !atomid } in
	 try H.find table f
	 with Not_found ->  (* print_endline(string_of_int(!atomid)); *)
	   incr atomid; H.add table f f; f
     let negation t = let (b,a,tl) = t.reveal in build (not b,a,tl)
     let toString t = match t.reveal with
       | (true,s, tl) -> "{"^s^Term.printtl(tl)^"}"
       | (false,s, tl) -> "\\non {"^s^"}"^Term.printtl(tl)
   end:sig
     module Predicates: sig
       type t
       val compare : t->t->int
     end
     type t
     val reveal: t -> bool*Predicates.t*(Term.t list)
     val build: bool*string*(Term.t list) -> t
     val id: t-> int       
     val negation: t -> t
     val toString: t-> string
     val equal: t->t->bool
     val hash: t -> int
   end)


type 'a form =
    Lit of Atom.t
  | AndP of 'a*'a
  | OrP of 'a*'a
  | AndN of 'a*'a
  | OrN of 'a*'a


(* Interface for an implementation of formulae *)

module type FormulaImplem = sig
  type t
  val reveal : t -> t form
  val build : t form -> t
end


(* Generic code providing standard functions about formulae *)

module PrintableFormula =
  functor (F: FormulaImplem) -> struct

    type t = F.t
	
    (* Displays a formula *)
    let rec toString f = match F.reveal f with
	Lit(l) -> Atom.toString l
      | AndN(f1,f2) -> "("^toString(f1)^") \\andN ("^toString(f2)^")"
      | OrN(f1,f2) -> "("^toString(f1)^") \\orN ("^toString(f2)^")"
      | AndP(f1,f2) -> "("^toString(f1)^") \\andP ("^toString(f2)^")"
      | OrP(f1,f2) -> "("^toString(f1)^") \\orP ("^toString(f2)^")"


    (* Negates a formula *)
    let rec negation f = F.build(
      match F.reveal f with
	| Lit t       -> Lit(Atom.negation t)
	| AndN(f1,f2) -> OrP(negation f1,negation f2)
	| OrN(f1,f2)  -> AndP(negation f1,negation f2)
	| AndP(f1,f2) -> OrN(negation f1,negation f2)
	| OrP(f1,f2)  -> AndN(negation f1,negation f2)
    )

    let lit(b,f,tl) = F.build(Lit(Atom.build (b,f,tl)))
    let andN(f1,f2) = F.build(AndN(f1,f2))
    let andP(f1,f2) = F.build(AndP(f1,f2))
    let orN(f1,f2)  = F.build(OrN(f1,f2))
    let orP(f1,f2)  = F.build(OrP(f1,f2))

  end


(* Default implementation for interface FormulaImplem *)

module MyFormulaImplem = 
  (struct
     type t = Reveal of t form
     let reveal (Reveal a) = a
     let build a = (Reveal a)
   end : FormulaImplem)

