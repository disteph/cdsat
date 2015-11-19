open Top
open Messages
open Specs

open Algo


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
      let c1 = Hashtbl.create 10 in
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace c1 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        let c2 = Hashtbl.create 10 in
        let g k v =
          try
            let temp = Hashtbl.find t1 k in
            Hashtbl.replace c2 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter g t2;
        Eqs([Equation.create c1 (s2 -/ s1) false []; Equation.create c2 (s1 -/ s2) false []])
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) ->
        let coeff = Hashtbl.create 10 in
        let f k v =
            Hashtbl.replace coeff k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create coeff (s2 -/ s1) false []; Equation.create t1 (s1 -/ s2) false []])
      | _, _ -> Other
      )
    | Symbols.NEq Sorts.Rat, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) true []; Equation.createFromList [] (s1 -/ s2) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
      let c1 = Hashtbl.create 10 in
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace c1 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        let c2 = Hashtbl.create 10 in
        let g k v =
          try
            let temp = Hashtbl.find t1 k in
            Hashtbl.replace c2 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter g t2;
        Eqs([Equation.create c1 (s2 -/ s1) true []; Equation.create c2 (s1 -/ s2) true []])
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) ->
        let coeff = Hashtbl.create 10 in
        let f k v =
            Hashtbl.replace coeff k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create coeff (s2 -/ s1) true []; Equation.create t1 (s1 -/ s2) true []])
      | _, _ -> Other
      )

    | Symbols.CstRat n, [] -> Cst(n)

    | Symbols.Le, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) false []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace t2 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create t1 (s2 -/ s1) false []])
      | ArithTerm(t1, s1), Cst(s2) -> Eqs([Equation.create t1 (s2 -/ s1) false []])
      | Cst(s1), ArithTerm(t2, s2) ->
        let f k v =
            Hashtbl.replace t2 k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t2;
        Eqs([Equation.create t2 (s2 -/ s1) false []])
      | _, _ -> Other
      )

    | Symbols.Lt, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s2 -/ s1) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace t2 k (v -/ temp)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create t1 (s2 -/ s1) true []])
      | ArithTerm(t1, s1), Cst(s2) -> Eqs([Equation.create t1 (s2 -/ s1) true []])
      | Cst(s1), ArithTerm(t2, s2) ->
        let f k v =
            Hashtbl.replace t2 k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t2;
        Eqs([Equation.create t2 (s2 -/ s1) true []])
      | _, _ -> Other
      )
    | Symbols.Ge, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s1 -/ s2) false []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace t2 k (temp -/ v)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create t1 (s1 -/ s2) false []])
      | ArithTerm(t1, s1), Cst(s2) ->
        let f k v =
          Hashtbl.replace t1 k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t1;
      Eqs([Equation.create t1 (s1 -/ s2) false []])
      | Cst(s1), ArithTerm(t2, s2) -> Eqs([Equation.create t2 (s1 -/ s2) false []])
      | _, _ -> Other
      )

    | Symbols.Gt, [a;b] -> (match a, b with
      | Cst(s1), Cst(s2) -> Eqs([Equation.createFromList [] (s1 -/ s2) true []])
      | ArithTerm(t1, s1), ArithTerm(t2, s2) ->
        let f k v =
          try
            let temp = Hashtbl.find t2 k in
            Hashtbl.replace t2 k (temp -/ v)
          with Not_found -> ()
        in
        Hashtbl.iter f t1;
        Eqs([Equation.create t1 (s1 -/ s2) true []])
      | ArithTerm(t1, s1), Cst(s2) ->
        let f k v =
          Hashtbl.replace t1 k (v */ (num_of_int (-1)))
        in
        Hashtbl.iter f t1;
      Eqs([Equation.create t1 (s1 -/ s2) true []])
      | Cst(s1), ArithTerm(t2, s2) -> Eqs([Equation.create t2 (s1 -/ s2) true []])
      | _, _ -> Other
      )

    | Symbols.Plus, [a;b] -> (match a,b with
      | ArithTerm(t1,s1), ArithTerm(t2, s2) ->
      let coeff = Hashtbl.copy t1 in
        let f k v =
          try
            let temp = Hashtbl.find coeff k in
            Hashtbl.replace coeff k (v +/ temp)
          with Not_found -> ()
        in
        (* loop through the coeffs of the first equation*)
        Hashtbl.iter f t2;
        ArithTerm(coeff, s1 +/ s2)
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) -> ArithTerm(t1, s1 +/ s2)
      | _, _ -> Other
      )

    | Symbols.Minus, [a;b] -> (match a,b with
      | ArithTerm(t1,s1), ArithTerm(t2, s2) ->
      let coeff = Hashtbl.copy t1 in
        let f k v =
          try
            let temp = Hashtbl.find coeff k in
            Hashtbl.replace coeff k (v -/ temp)
          with Not_found -> ()
        in
        (* loop through the coeffs of the first equation*)
        Hashtbl.iter f t2;
        ArithTerm(coeff, s1 -/ s2)
      | ArithTerm(t1, s1), Cst(s2) | Cst(s2), ArithTerm(t1, s1) -> ArithTerm(t1, s1 -/ s2)
      | _, _ -> Other
      )

    | Symbols.Times, [a;b] -> (match a,b with
      | ArithTerm(t1,s1), Cst(s2) | Cst(s2), ArithTerm(t1,s1) ->
      let coeff = Hashtbl.create 10 in
        let f k v =
            Hashtbl.replace coeff k (v */ s2)
        in
        (* loop through the coeffs of the first equation*)
        Hashtbl.iter f t1;
        ArithTerm(coeff, s1 -/ s2)
      | _, _ -> Other
      )

    | Symbols.Divide, [a;b] -> (match a,b with
      | ArithTerm(t1,s1), Cst(s2) ->
      let coeff = Hashtbl.create 10 in
        let f k v =
            Hashtbl.replace coeff k (v // s2)
        in
        (* loop through the coeffs of the first equation*)
        Hashtbl.iter f t1;
        ArithTerm(coeff, s1 -/ s2)
      | _, _ -> Other
      )

    | Symbols.Op, [a;b] -> (match a,b with
      | ArithTerm(t1,s1),_ ->
      let coeff = Hashtbl.create 10 in
        let f k v =
            Hashtbl.replace coeff k (v */ (num_of_int (-1)))
        in
        (* loop through the coeffs of the first equation*)
        Hashtbl.iter f t1;
        ArithTerm(coeff, s1*/ (num_of_int (-1)))
      | Cst(s),_ -> Cst((num_of_int (-1))*/s)
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
