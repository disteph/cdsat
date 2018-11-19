open Format
   
open General
open Patricia
open Patricia_tools

open Top
open Basic
open Messages
open Terms
open Values
open Sassigns

open Theory

module Sgn = struct
  include Circuit

  (* Predicate "is true ?" *)
  let isT a = let b=(a ==: vdd) in Signal.isT(b)
  let equal a b = isT(a ==: b)
  let compare a b = if equal a b then 0
                    else if isT(a <=: b) then 1 else -1
  (* hash function only uses the lowest 16 bits: to be refined later? *)
  let hash a =to_int(select a 16 0) 
  let hash_fold_t = Hash.hash2fold hash
  let pp fmt a =
    let rec aux fmt = function
      | [] -> ()
      | t::q -> fprintf fmt "%s%a" (if t then "1" else "0") aux q
    in
    aux fmt (List.map isT (bits a))
  let show = Print.stringOf pp
  let name = "BV"
end
           

module T = struct

  (* evaluation of a term *)
  let term_eval valuation term =
    let rec aux term =
      match Term.reveal term with
      | Terms.C(Symbols.Extract{hi;lo},[a]) ->
         let a = aux a in
         Sgn.select_e a hi lo
      | Terms.C(Symbols.Conc _,[a;b]) ->
         let a = aux a in
         let b = aux b in
         Sgn.(a @: b)
      | Terms.C(Symbols.CstBV s,[]) ->
         Sgn.constb s
      | _ -> let v = valuation term in
             match v with
             | None -> Signal.id (*n*) (*n is the dimension of the bitvector, unknown for now*)
             | vv -> Signal.cast v
    in aux term

  (* Exception that we raise if we cannot evaluate a bitvector formula *)
  exception CannotEval of string

  (* evaluation of an atomic predicate *)
  let form_eval valuation form =
    let term_eval = term_eval valuation in
    match Term.reveal form with
    | Terms.C(Symbols.Eq(Sorts.BV _),[a;b]) ->
       let a = term_eval a in
       let b = term_eval b in
       Sgn.equal a b
    | Terms.C(Symbols.NEq(Sorts.BV _),[a;b]) ->
       let a = term_eval a in
       let b = term_eval b in
       not(Sgn.equal a b)
    | _ -> raise(CannotEval(Print.stringOf Term.pp form))

  (* Evaluation function for a formula, given the state of the algorithm.
       Either raises an exception CannotEval,
       or produces a message Propa(assign,boolassign), where
       boolassign is the Boolean assignment saying if the formula evaluates to true or false
       assign are the assignments that were used in the evaluation *)
  let eval assign f term =
    match Term.get_sort term with
    | Sorts.Prop ->
       let b = form_eval f term
       in straight () assign (term,Values.Boolean b)
    (* | Sorts.BV _ -> *)
    (*    let v,assign = term_eval_by f by term *)
    (*    in Values.NonBoolean v, assign  *)
    | s -> raise (CannotEval(Format.toString(fun p ->
                                 p "I do not know sort %a" Sorts.pp s)))

end
