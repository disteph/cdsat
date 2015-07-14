open Format

open Top
open Symbol
open Interfaces_basic
open Basic
open Messages

open Formulae

type t =
| TermI : LitB.Term.t -> t
| PropI : FormulaB.t -> t

open FormulaB

let leaf v = TermI(LitB.Term.bV v)

let toForm = function
  | PropI f -> f
  | _       -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

let toTerm = function
  | TermI t -> t
  | _       -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

let semantic_aux symb l =
  match symb, l with
  | True,[]       -> trueN
  | False,[]      -> falseN
  | Neg,[a]       -> negation a
  | And,[a;b]     -> andP(a,b)
  | Or, [a;b]     -> orN(a,b)
  | Imp,[a;b]     -> orN(negation a,b)
  | Xor,[a;b]     -> andP(orN(a,b),orN(negation a,negation b))
  | Eq Sorts.Prop, [a;b] -> andP(orN(negation a,b),orN(negation b,a))
  | NEq Sorts.Prop,[a;b] -> orN(andP(a,negation b),andP(b,negation a))
  | Forall so,[a]  -> forall(so,a)
  | Exists so,[a]  -> exists(so,a)
    (* | ITEProp,[a;b;c] -> bool_simpl(INode(a,Leaf b,Leaf c)) *)
  | _             -> raise (ModelError "ModelError: semantic_aux does not know symbol")


let semantic symb =
  let (o,_) = Symbol.arity symb in
  match o with
  | Sorts.Prop -> fun l -> 
    PropI (
      try semantic_aux symb (List.map toForm l)
      with ModelError _
        -> lit(true,symb,List.map toTerm l)
    )
  | _ -> fun l -> 
    TermI (LitB.Term.bC symb (List.map toTerm l))
