type _ t =
  | NoRep   : unit t
  | LitF    : Literals.TS.t t
  | Clauses : Clauses.TS.t t
  | Rationals : Rationals.TS.t t
  | VarCheck : Varcheck.TS.t t
                               
let equal (type a) (type b) : a t -> b t -> (a -> b) option =
  fun ts1 ts2 ->
  match ts1,ts2 with
  | NoRep, NoRep    -> Some(fun x->x)
  | LitF, LitF      -> Some(fun x->x)
  | Clauses,Clauses -> Some(fun x->x)
  | Rationals, Rationals -> Some(fun x->x)
  | VarCheck,VarCheck -> Some(fun x->x)
  | _ -> None

type _ get =
  | NoRepModule : unit get
  | RepModule   : (module Top.Specs.DataType with type t = 'a) -> 'a get
           
let get (type a) : a t -> a get = function
  | NoRep     -> NoRepModule
  | LitF      -> RepModule(module Literals.TS)
  | Clauses   -> RepModule(module Clauses.TS)
  | Rationals -> RepModule(module Rationals.TS)
  | VarCheck  -> RepModule(module Varcheck.TS)
