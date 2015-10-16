open Format

open Top
open Symbols
open Interfaces_basic
open Basic
open Specs

open Literals
open Formulae

type t =
| TermI : TermB.t -> t
| PropI : FormulaB.t -> t

open FormulaB

let bV v = TermI(TermB.bV v)

let toForm = function
  | PropI f -> f
  | _       -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

let toTerm = function
  | TermI t -> t
  | _       -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

let bC_aux symb l =
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
  | ITE Sorts.Prop,[a;b;c] -> orN(andP(a,b),andP(negation a,c))
  | _             -> raise (ModelError "ModelError: bc_aux does not know symbol")


let bC symb l =
  try PropI(bC_aux symb (List.map toForm l))
  with ModelError _
    -> let term = TermB.bC symb (List.map toTerm l) in
       let (o,_) = arity symb in
       match o with
       | Sorts.Prop -> PropI(lit(true,term))
       | _          -> TermI(term)
