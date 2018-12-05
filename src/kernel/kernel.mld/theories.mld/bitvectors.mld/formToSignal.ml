open Top
open Terms
open Values
    
(* evaluation of a term *)
let term_eval valuation term =
  let open Symbols in
  let open Terms in
  let valuation = Eq.Valuation.reveal valuation in
  let open Circuit in
  let rec aux term =
    match Term.reveal term with
    | C(BVextract{hi;lo},[a]) -> let a = aux a in select_e a hi lo
    | C(BVconc _,[a;b]) -> let a = aux a and b = aux b in (a @: b)
    | C(BVcst v,[])     -> Signal.cast v
    | C(BVnot v,[a])    -> let a = aux a in ( ~: a)
    | C(BVneg v,[a])    -> let a = aux a in negate a
    | C(BVand _,[a;b])  -> let a = aux a and b = aux b in (a &&: b)
    | C(BVor  _,[a;b])  -> let a = aux a and b = aux b in (a ||: b)
    | C(BVxor _,[a;b])  -> let a = aux a and b = aux b in (a ^: b)
    | C(BVadd _,[a;b])  -> let a = aux a and b = aux b in (a +: b)
    | C(BVmul _,[a;b])  -> let a = aux a and b = aux b in (a *: b)
    | _ when Eq.Valuation.mem term valuation -> 
      let v,_ = Eq.Valuation.find term valuation in
      begin match CValue.proj MyTheory.vkey v with
        | Some(Values(NonBoolean v)) -> Signal.cast v
        | _ -> failwith "Egraph lied"
      end      
    | _ -> match Term.get_sort term with
      | BV n -> Signal.id n
      | _ -> failwith "Not a bitvector."
  in aux term

(* Exception that we raise if we cannot evaluate a bitvector formula *)
exception CannotEval of string

(* evaluation of an atomic predicate, with an optional truth value *)
let form_eval valuation ?(truthvalue=true) form =
  let term_eval = term_eval valuation in
  let result b a1 a2 =
    let open Circuit in
    let sign = (term_eval a1) ==: (term_eval a2) in
    if b then sign else ~: sign
  in
  match Term.reveal form with
  | Terms.C(Symbols.Eq(Sorts.BV _),[a;b])  ->  result truthvalue a b   
  | Terms.C(Symbols.NEq(Sorts.BV _),[a;b]) ->  result (not truthvalue) a b
  | _ -> raise(CannotEval(Format.stringOf Term.pp form))
