type _ t =
  | NoRep   : unit t
  | VarSetEq: VarSet.Eq.t t
  | VarSetBV: VarSet.BV.t t
  | VarSetITE: VarSet.ITE.t t
  | VarSetArrays: VarSet.Arrays.t t
  | LitF    : Literals.TS.t t
  | Clauses : Clauses.TS.t t
  | Rationals : Rationals.TS.t t

val equal : 'a t -> 'b t -> ('a,'b) PolyEq.t

type _ get =
  | NoRepModule : unit get
  | RepModule   : (module Top.Specs.DataType with type t = 'a) -> 'a get
           
val get : 'a t -> 'a get
