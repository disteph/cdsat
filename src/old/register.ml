(* Type of all term representation handlers *)

type _ t =
  | NoRep   : unit t
  | VarSetEq: VarSet.Eq.t t
  | VarSetBV: VarSet.BV.t t
  | VarSetITE: VarSet.ITE.t t
  | VarSetArrays: VarSet.Arrays.t t
  | LitF    : Literals.TS.t t
  | Clauses : Clauses.TS.t t
  | Rationals : Rationals.TS.t t
                               
let equal (type a b) : a t -> b t -> (a -> b) option =
  fun ts1 ts2 ->
  match ts1,ts2 with
  | NoRep, NoRep    -> Some(fun x->x)
  | VarSetEq, VarSetEq -> Some(fun x->x)
  | VarSetBV, VarSetBV -> Some(fun x->x)
  | VarSetITE, VarSetITE -> Some(fun x->x)
  | VarSetArrays, VarSetArrays -> Some(fun x->x)
  | LitF, LitF      -> Some(fun x->x)
  | Clauses,Clauses -> Some(fun x->x)
  | Rationals, Rationals -> Some(fun x->x)
  | _ -> None

type _ get =
  | NoRepModule : unit get
  | RepModule   : (module Top.Specs.DataType with type t = 'a) -> 'a get
           
let get (type a) : a t -> a get = function
  | NoRep     -> NoRepModule
  | VarSetEq  -> RepModule(module VarSet.Eq)
  | VarSetBV  -> RepModule(module VarSet.BV)
  | VarSetITE -> RepModule(module VarSet.ITE)
  | VarSetArrays -> RepModule(module VarSet.Arrays)
  | LitF      -> RepModule(module Literals.TS)
  | Clauses   -> RepModule(module Clauses.TS)
  | Rationals -> RepModule(module Rationals.TS)
