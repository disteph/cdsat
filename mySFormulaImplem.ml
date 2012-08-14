open Formulae

type tt = {reveal: tt form;id:int;size:int}

let id f = f.id
  
(* HashedType for formulae *)

module MySmartFormulaImplemPrimitive = 
  (struct
     type t = tt
     let equal t1 t2 =
       (* print_endline "equal"; *)
       match t1.reveal,t2.reveal with
	 | Lit l1, Lit l2             -> l1==l2
	 | AndP (x1,x2), AndP (y1,y2) -> x1==y1 && x2==y2
	 | OrP (x1,x2), OrP (y1,y2)   -> x1==y1 && x2==y2
	 | AndN (x1,x2), AndN (y1,y2) -> x1==y1 && x2==y2
	 | OrN (x1,x2), OrN (y1,y2)   -> x1==y1 && x2==y2
	 | _                          -> false 
     let hash t1 =
       (* print_endline "hash"; *)
       match t1.reveal with
	 | Lit l        -> Atom.hash l
	 | AndP (x1,x2) -> 5*x1.id+17*x2.id
	 | OrP (x1,x2)  -> 7*x1.id+19*x2.id
	 | AndN (x1,x2) -> 11*x1.id+23*x2.id
	 | OrN (x1,x2)  -> 13*x1.id+29*x2.id
   end: Hashtbl.HashedType with type t=tt)

include MySmartFormulaImplemPrimitive

module H = Hashtbl.Make(MySmartFormulaImplemPrimitive)

(* Computes the size of the formula *)

let prior = function
  | Lit l        -> - 1
  | AndP (x1,x2) -> - x1.size - x2.size
  | OrP (x1,x2)  -> - x1.size - x2.size
  | AndN (x1,x2) -> - x1.size - x2.size
  | OrN (x1,x2)  -> - x1.size - x2.size
      
(* Constructing a formula with HConsing techniques *)

let table = H.create 5003
let unique =ref 0

module FI = struct
  type t = tt
  let build a =
    let f = {reveal =  a; id = !unique; size = prior a} in
      try H.find table f
      with Not_found -> incr unique; H.add table f f; f

  let reveal f = f.reveal
end

let build  = FI.build
let reveal = FI.reveal

let compare t1 t2 =
  let a = (Pervasives.compare t1.size t2.size) in
    if (a<>0) then  a else Pervasives.compare t1.id t2.id

let clear() = H.clear table
