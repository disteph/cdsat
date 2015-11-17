open Top
open Messages
open Specs

open Algo


module ThDS = struct

  type t =
    | Cst of num
    | ArithTerm of (Equation.var, Equation.value)Hashtbl.t * num
    | Eq of Equation.equation
    | Other

  let bV tag fv = match Variables.FreeVar.get_sort fv with
    | Sorts.Rat -> let coeff = Hashtbl.create 10 in Hashtbl.add coeff tag (num_of_int 1); ArithTerm(coeff,num_of_int 0)
    | _ -> Other

  let bC tag symb l = match symb, l with
    | Symbols.Neg, [a] -> (match a with
      | Eq eq -> Eq(Equation.toggleStrict (Equation.multiply eq (num_of_int (-1))))
      | _ -> Other
      )

    | Symbols.Eq Sorts.Rat, l -> failwith "TODO"
    | Symbols.NEq Sorts.Rat, l -> failwith "TODO"
    | Symbols.CstRat n, [] -> Cst(n)
    | Symbols.Ge, l -> failwith "TODO"
    | Symbols.Le, l -> failwith "TODO"
    | Symbols.Gt, l -> failwith "TODO"
    | Symbols.Lt, l -> failwith "TODO"

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
