open Kernel
open Interfaces
open Lib
open SetConstructions
open MyAtom
open Patricia

(* the module X for CC(X) where X=Empty *)

module Sig    = ThSig_register.CCemptySig
module Atom   = Atom

module Structure(F:PrintableFormulaType with type lit = Atom.t)
  = struct

    module PS = ThDecProc_tools.PropStructure(F)

    open Theories

    type t = 
    | Prop of F.t
    | Ter of Term.t

    let lit (b, f, tl) = F.lit(Atom.bbuild (b, f, tl))

    let toform = function
      | Prop f -> f
      | _      -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

    let toterm = function
      | Ter t -> t
      | _ -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

    let st = 
      { sigsymb_i = 
          (fun symb l ->
	    match symb,l with
	    | `EqTerm, [Ter a; Ter b] -> Prop (lit (true,"=",[a;b]))
	    | `NEqTerm,[Ter a; Ter b] -> Prop (lit (false,"=",[a;b]))
	    | s,l -> Prop (PS.symb_i s (List.map toform l))
	  );
	decsymb_i =
          function
	  | `Prop -> fun (var:string) l -> 
	    Prop (lit (true,var,List.map toterm l))
	  | `Term -> fun (var:string) l -> 
	    Ter (Term.build (Term.C (var,List.map toterm l)))
	  | _     -> fun (var:string) -> raise (ModelError ("ModelError: variable "^var^" not of expected type `Prop or `Term"))
      }

    let	examples = []

  end

type t = Term.t

let print_term t = print_string ((Term.toString t)^" ")

let toString = Term.toString

module F = struct
  type t = Term.t
  let id f = Term.id f
end

module I = TypesFromHConsed (F)

let compare f1 f2 = Pervasives.compare (F.id f1) (F.id f2)

module DTSet = struct
  type keys = t
  let kcompare = compare
  type values = unit
  let vcompare () () = 0
  type infos = unit
  let info_build = ((),(fun f v -> ()),fun x y -> ())
  let treeHCons = true
end

module TSet = struct
  include PATSet (DTSet) (I)
  type e = Term.t
  let is_in = mem
end

let directSubterms t = 
  match (Term.reveal t) with
  | (Term.V x) | (Term.XV x) -> []
  | (Term.C(f,l)) -> l

let root t = 
  match (Term.reveal t) with
  | (Term.V x) | (Term.XV x) -> ""
  | (Term.C(f,l)) -> f

let predicate t = 
  let (b,p,s) = Atom.reveal t in
  (b,Atom.Predicates.reveal p,s)

let build (b,p,s) =
  Atom.bbuild (b,p,s)

(* the semantic values are terms: 
   there is no interpreted symbol for this theory *)
type v = t

let print_value = print_term

module VSet = TSet

module DVtoTSet = struct
  type keys = t
  let kcompare = compare
  type values = TSet.t
  let vcompare = TSet.compare
  type infos = unit
  let info_build = ((),(fun f v -> ()),fun x y -> ())
  let treeHCons = true
end

module MVtoTSet = PATMap (DVtoTSet) (I)

module VtoTSet = struct
  type e = t
  type v = TSet.t
  type t = MVtoTSet.t
  let find = MVtoTSet.find
  let empty = MVtoTSet.build MVtoTSet.Empty
  let add i s t = MVtoTSet.add i (fun f -> match f with
    | None -> s
    | (Some r) -> s) t
  let union t t' = MVtoTSet.union (fun s s' -> TSet.union s s') t t'
  let remove i t = MVtoTSet.remove i t
  let map f t = MVtoTSet.map (fun x y -> f y) t
  let fold f t a = MVtoTSet.fold (fun x y a' -> f x a') t a
end

module DVtoV = struct
  type keys = t
  let kcompare = compare
  type values = t
  let vcompare = compare
  type infos = unit
  let info_build = ((),(fun f v -> ()),fun x y -> ())
  let treeHCons = true
end

module MVtoV = PATMap (DVtoV) (I)

module VtoV = struct
  type e = t
  type v = t
  type t = MVtoV.t
  let find = MVtoV.find
  let empty = MVtoV.build MVtoV.Empty
  let add i s t = MVtoV.add i (fun f -> match f with
    | None -> s
    | (Some r) -> s) t
  let union t t' = MVtoV.union (fun s s' -> s) t t'
  let remove i t = MVtoV.remove i t
  let map f t = MVtoV.map (fun x y -> f y) t
  let fold f t a = MVtoV.fold (fun x y a' -> f x a') t a
end

let make t = t

let leaves r = (VSet.add r VSet.empty)

let rec subst p q r =
  if (compare p r) = 0 then q else r

type  input = 
| Eq of t*t
| NEq of t*t
| IsEq of t*t
| IsNEq of t*t
| Congr of t*t
    
type res = Sol of v*v | Bot | Top

let solve r r' =
  if (compare r r') = 0 then Top
  else Sol(r,r')

let atoI a = 
  match (Atom.reveal a) with
  | (b,p,l) when (Atom.Predicates.reveal p) = "=" -> 
    if b then (Eq(List.hd l,List.hd (List.tl l)))
    else (NEq(List.hd l,List.hd (List.tl l)))
  | _ -> assert false

let itoA = function
  | (Eq(a,b)) -> Atom.bbuild (true,"=",[a;b])
  | (NEq(a,b)) -> Atom.bbuild (false,"=",[a;b])
  | _ -> assert false


