open Kernel
open Basic
open Interfaces_basic

open General
open SetConstructions
open Patricia

open Theories_tools
open ForGround
open StandardStruct

(* the module X for CC(X) where X=Empty *)


(* let st =  *)
(*   { sigsymb_i =  *)
(*       (fun symb l -> *)
(* 	match symb,l with *)
(* 	| `EqTerm, [Ter a; Ter b] -> Prop (lit (true,"=",[a;b])) *)
(* 	| `NEqTerm,[Ter a; Ter b] -> Prop (lit (false,"=",[a;b])) *)
(* 	| s,l -> Prop (PS.symb_i s (List.map toform l)) *)
(*       ); *)
(*     decsymb_i = *)
(*       (function *)
(*       | `Prop -> fun (var:string) l ->  *)
(* 	Prop (lit (true,var,List.map toterm l)) *)
(*       | `Term -> fun (var:string) l ->  *)
(* 	Ter (Term.build (Term.C (var,List.map toterm l))) *)
(*       | _     -> fun (var:string) -> raise (ModelError ("ModelError: variable "^var^" not of expected type `Prop or `Term"))); *)
(*     boundsymb_i = (fun db so -> raise (ModelError ("ModelError: cannot treat bound variables"))); *)
(*     quantif_i = (fun db so sf -> raise (ModelError ("ModelError: cannot treat quantifiers"))) *)
(*   } *)

module ED = EmptyData(IntSort)

include StandardDS(IntSort)(ED)

module Term = Atom.Term

type t = Term.t

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
  let info_build = empty_info_build
  let treeHCons = Some(Term.id,fun()->0)
end

module TSet = struct
  include PATSet (DTSet) (I)
  type e = Term.t
  let is_in = mem
end

let directSubterms t = 
  match (TermDef.reveal t) with
  | (TermDef.V x)    -> []
  | (TermDef.C(f,l)) -> l

let root t = 
  match (TermDef.reveal t) with
  | (TermDef.V x)    -> None
  | (TermDef.C(f,l)) -> Some f

let predicate t = 
  let (b,p,s) = AtomDef.reveal t in
  (b,Predicates.reveal p,s)

let build (b,p,s) =
  Atom.bbuild (b,p,s)

(* the semantic values are terms: 
   there is no interpreted symbol for this theory *)
type v = t

module VSet = TSet

module DVtoTSet = struct
  type keys = t
  let kcompare = compare
  type values = TSet.t
  let vcompare = TSet.compare
  type infos = unit
  let info_build = empty_info_build
  let treeHCons = Some(Term.id,TSet.id)
end

module MVtoTSet = PATMap (DVtoTSet) (I)

module VtoTSet = struct
  type e = t
  type v = TSet.t
  type t = MVtoTSet.t
  let find = MVtoTSet.find
  let empty = MVtoTSet.empty
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
  let info_build = empty_info_build
  let treeHCons = Some(Term.id,Term.id)
end

module MVtoV = PATMap (DVtoV) (I)

module VtoV = struct
  type e = t
  type v = t
  type t = MVtoV.t
  let find = MVtoV.find
  let empty = MVtoV.empty
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
  match AtomDef.reveal a with
  | (b,p,l) ->
    begin
      match Predicates.reveal p with
      | `Eq _ -> 
        if b then (Eq(List.hd l,List.hd (List.tl l)))
        else (NEq(List.hd l,List.hd (List.tl l)))
      | _ -> assert false
    end
  | _ -> assert false

let itoA = function
  | Eq(a,b)  -> Atom.bbuild (true,`Eq (Sorts.User ""),[a;b])
  | NEq(a,b) -> Atom.bbuild (false,`Eq (Sorts.User ""),[a;b])
  | _ -> assert false


