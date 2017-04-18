open Num

type alt =
  | Cst of num
  | ArithTerm of (Equation.var, Equation.value)Hashtbl.t * num
  | Ineq of Equation.equation
  | EqNeq of bool * int
  | Other

include Top.Specs.DataType with type t = alt
