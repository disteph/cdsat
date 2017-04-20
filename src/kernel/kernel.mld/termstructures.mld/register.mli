type _ t =
  | NoRep   : unit t
  | LitF    : Literals.TS.t t
  | Clauses : Clauses.TS.t t
  | Rationals : Rationals.TS.t t
  | VarCheck : Varcheck.TS.t t

val equal : 'a t -> 'b t -> ('a -> 'b) option

type _ get =
  | NoRepModule : unit get
  | RepModule   : (module Top.Specs.DataType with type t = 'a) -> 'a get
           
val get : 'a t -> 'a get
