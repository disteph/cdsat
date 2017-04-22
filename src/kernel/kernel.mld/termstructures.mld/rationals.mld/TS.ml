open Num

open Top

(* Apply the operation op to two hashtable, to value with equal key *)
let applyOp2 op t1 t2 =
  let res = Hashtbl.copy t1 in
  let g k v =
    try
      let temp = Hashtbl.find t1 k in
      Hashtbl.replace res k (op temp v)
    with Not_found -> Hashtbl.replace res k (op (num_of_int 0) v)
  in
  Hashtbl.iter g t2;
  res

(* Create a hashtable whose value are the opposite of those of the original one *)
let opposite t1 =
  let res = Hashtbl.create 10 in
  let f k v =
    Hashtbl.replace res k (v */ (num_of_int (-1)))
  in
  Hashtbl.iter f t1;
  res

type alt =
  | Cst of num
  | ArithTerm of (Equation.var, Equation.value)Hashtbl.t * num
  | Ineq of Equation.equation
  | EqNeq of bool * int
  | Other

type t = alt

let bV tag fv = match Variables.FreeVar.get_sort fv with
  | Sorts.Rat -> let coeff = Hashtbl.create 10 in Hashtbl.add coeff tag (num_of_int 1); ArithTerm(coeff,num_of_int 0)
  | _ -> Other

let bC tag symb l = match symb, l with

  | Symbols.Neg, [a] -> (match a with
    | Ineq eq    -> Ineq(Equation.toggleStrict (Equation.multiply eq (num_of_int (-1))))
    | EqNeq(b,i) -> EqNeq(not b,i)
    | _ -> Other
  )

  | Symbols.Eq Sorts.Rat, [a;b]
  | Symbols.NEq Sorts.Rat, [a;b] 
    -> EqNeq(true, tag)

  | Symbols.CstRat n, [] -> Cst(n)

  | Symbols.Le, [a;b] -> (match a, b with
    | Cst(s1), Cst(s2) -> Ineq(Equation.createFromList [] (s2 -/ s1) false [] (Some tag))
    | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
       let c = applyOp2 (-/) t1 t2 in
       Ineq(Equation.create c (s2 -/ s1) false [] (Some tag))
    | ArithTerm(t1, s1), Cst(s2) -> Ineq(Equation.create t1 (s2 -/ s1) false [] (Some tag))
    | Cst(s1), ArithTerm(t2, s2) ->
       let c = opposite t2 in
       Ineq(Equation.create c (s2 -/ s1) false [] (Some tag))
    | _, _ -> Other
  )

  | Symbols.Lt, [a;b] -> (match a, b with
    | Cst(s1), Cst(s2) -> Ineq(Equation.createFromList [] (s2 -/ s1) true [] (Some tag))
    | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
       let c = applyOp2 (-/) t1 t2 in
       Ineq(Equation.create c (s2 -/ s1) true [] (Some tag))
    | ArithTerm(t1, s1), Cst(s2) -> Ineq(Equation.create t1 (s2 -/ s1) true [] (Some tag))
    | Cst(s1), ArithTerm(t2, s2) ->
       let c = opposite t2 in
       Ineq(Equation.create c (s2 -/ s1) true [] (Some tag))
    | _, _ -> Other
  )

  | Symbols.Ge, [a;b] -> (match a, b with
    | Cst(s1), Cst(s2) -> Ineq(Equation.createFromList [] (s1 -/ s2) false [] (Some tag))
    | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
       let c = applyOp2 (-/) t2 t1 in
       Ineq(Equation.create c (s1 -/ s2) false [] (Some tag))
    | ArithTerm(t1, s1), Cst(s2) ->
       let c = opposite t1 in
       Ineq(Equation.create c (s1 -/ s2) false [] (Some tag))
    | Cst(s1), ArithTerm(t2, s2) -> Ineq(Equation.create t2 (s1 -/ s2) false [] (Some tag))
    | _, _ -> Other
  )

  | Symbols.Gt, [a;b] -> (match a, b with
    | Cst(s1), Cst(s2) -> Ineq(Equation.createFromList [] (s1 -/ s2) true [] (Some tag))
    | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
       let c = applyOp2 (-/) t2 t1 in
       Ineq(Equation.create c (s1 -/ s2) true [] (Some tag))
    | ArithTerm(t1, s1), Cst(s2) ->
       let c = opposite t1 in
       Ineq(Equation.create c (s1 -/ s2) true [] (Some tag))
    | Cst(s1), ArithTerm(t2, s2) -> Ineq(Equation.create t2 (s1 -/ s2) true [] (Some tag))
    | _, _ -> Other
  )

  | Symbols.Plus, [a;b] -> (match a,b with
    | ArithTerm(t1,s1), ArithTerm(t2, s2) ->
       let coeff = applyOp2 (+/) t1 t2 in
       ArithTerm(coeff, s1 +/ s2)
    | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) -> ArithTerm(t1, s1 +/ s2)
    | _, _ -> Other
  )

  | Symbols.Minus, [a;b] -> (match a,b with
    | ArithTerm(t1,s1), ArithTerm(t2, s2) ->
       let coeff = applyOp2 (-/) t1 t2 in
       ArithTerm(coeff, s1 -/ s2)
    | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) -> ArithTerm(t1, s1 -/ s2)
    | _, _ -> Other
  )

  | Symbols.Times, [a;b] -> (match a,b with
    | ArithTerm(t1,s1), Cst(s2) | Cst(s2), ArithTerm(t1,s1) ->
       let coeff = Hashtbl.create 10 in
       Hashtbl.iter (fun k v -> Hashtbl.replace coeff k (v */ s2)) t1;
       ArithTerm(coeff, s1 */ s2)
    | Cst(s1), Cst(s2) -> Cst(s1 */ s2)
    | _, _ -> Other
  )

  | Symbols.Divide, [a;b] -> (match a,b with
    | ArithTerm(t1,s1), Cst(s2) ->
       let coeff = Hashtbl.create 10 in
       Hashtbl.iter (fun k v -> Hashtbl.replace coeff k (v // s2)) t1;
       ArithTerm(coeff, s1 */ s2)
    | Cst(s1), Cst(s2) -> Cst(s1 // s2)
    | _, _ -> Other
  )

  | Symbols.Op, [a] -> (match a with
    | ArithTerm(t1,s1) ->
       let coeff = opposite t1 in
       ArithTerm(coeff, s1 */ (num_of_int (-1)))
    | Cst(s) -> Cst((num_of_int (-1))*/s)
    | _ -> Other
  )

  | _,_ -> begin
    match Symbols.arity symb with
    | Sorts.Rat, _ -> let coeff = Hashtbl.create 10 in Hashtbl.add coeff tag (num_of_int 1); ArithTerm(coeff,num_of_int 0)
    | _,_ -> Other
  end

let bB _ _ = Other
