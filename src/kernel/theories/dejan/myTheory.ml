open Top
open Messages
open Specs

open Algo

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

module ThDS = struct

  type t =
    | Cst of num
    | ArithTerm of (Equation.var, Equation.value)Hashtbl.t * num
    | Eqs of Equation.equation list
    | Other

  let bV tag fv = match Variables.FreeVar.get_sort fv with
    | Sorts.Rat -> let coeff = Hashtbl.create 10 in Hashtbl.add coeff tag (num_of_int 1); ArithTerm(coeff,num_of_int 0)
    | _ -> Other

  let bC tag symb l = match symb, l with
    | Symbols.Neg, [a] -> (match a with
      | Eqs [eq] -> Eqs([Equation.toggleStrict (Equation.multiply eq (num_of_int (-1)))])
      | _ -> Other
      )

    | Symbols.Eq Sorts.Rat, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) false []; Equation.createFromList [] (s1 -/ s2) false []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c1 = applyOp2 (-/) t1 t2 in
        let c2 = applyOp2 (-/) t2 t1 in
        Eqs([Equation.create c1 (s2 -/ s1) false []; Equation.create c2 (s1 -/ s2) false []])
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) ->
        let coeff = opposite t1 in
        Eqs([Equation.create coeff (s1 -/ s2) false []; Equation.create t1 (s2 -/ s1) false []])
      | _, _ -> Other
      )

    | Symbols.NEq Sorts.Rat, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) true []; Equation.createFromList [] (s1 -/ s2) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c1 = applyOp2 (-/) t1 t2 in
        let c2 = applyOp2 (-/) t2 t1 in
        Eqs([Equation.create c1 (s2 -/ s1) true []; Equation.create c2 (s1 -/ s2) true []])
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) ->
        let coeff = opposite t1 in
        Eqs([Equation.create coeff (s1 -/ s2) true []; Equation.create t1 (s2 -/ s1) true []])
      | _, _ -> Other
      )

    | Symbols.CstRat n, [] -> Cst(n)

    | Symbols.Le, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) false []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c = applyOp2 (-/) t1 t2 in
        Eqs([Equation.create c (s2 -/ s1) false []])
      | ArithTerm(t1, s1), Cst(s2) -> Eqs([Equation.create t1 (s2 -/ s1) false []])
      | Cst(s1), ArithTerm(t2, s2) ->
        let c = opposite t2 in
        Eqs([Equation.create c (s2 -/ s1) false []])
      | _, _ -> Other
      )

    | Symbols.Lt, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c = applyOp2 (-/) t1 t2 in
          Eqs([Equation.create c (s2 -/ s1) true []])
      | ArithTerm(t1, s1), Cst(s2) -> Eqs([Equation.create t1 (s2 -/ s1) true []])
      | Cst(s1), ArithTerm(t2, s2) ->
        let c = opposite t2 in
          Eqs([Equation.create c (s2 -/ s1) true []])
      | _, _ -> Other
      )

    | Symbols.Ge, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s1 -/ s2) false []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c = applyOp2 (-/) t2 t1 in
          Eqs([Equation.create c (s1 -/ s2) false []])
      | ArithTerm(t1, s1), Cst(s2) ->
        let c = opposite t1 in
          Eqs([Equation.create c (s1 -/ s2) false []])
      | Cst(s1), ArithTerm(t2, s2) -> Eqs([Equation.create t2 (s1 -/ s2) false []])
      | _, _ -> Other
      )

    | Symbols.Gt, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s1 -/ s2) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let c = applyOp2 (-/) t2 t1 in
          Eqs([Equation.create c (s1 -/ s2) true []])
      | ArithTerm(t1, s1), Cst(s2) ->
        let c = opposite t1 in
          Eqs([Equation.create c (s1 -/ s2) true []])
      | Cst(s1), ArithTerm(t2, s2) -> Eqs([Equation.create t2 (s1 -/ s2) true []])
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

end

type sign = unit

module Make(DS: sig
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) = struct

  open DS

  let rec state atomN =
    (module struct

      type newoutput = (sign,TSet.t) output
      type tset = TSet.t

      let treated () = atomN

      let add = function
        | None -> Output(None,state atomN)
        | Some tset ->
           let newtreated = TSet.union atomN tset in
           Output(Some(thNotProvable () newtreated),state newtreated)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, state atomN)

    end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)

  let init = state TSet.empty

end
