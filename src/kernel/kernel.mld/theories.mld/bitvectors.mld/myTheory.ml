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
    
type sign = unit
  
(* This is the module for constant bitvectors *)
module CstBV = HardCaml.Bits.Comb.ArraybitsNativeint
              
module V = struct
  include CstBV

  (* Predicate "is true ?" *)
  let isT a = [%eq:nativeint array * int] (a ==: vdd) vdd
  let equal a b = isT(a ==: b)
  let compare a b = if equal a b then 0
                    else if isT(a <=: b) then 1 else -1
  (* hash function only uses the lowest 16 bits: to be refined later? *)
  let hash a = CstBV.to_int(CstBV.select a 16 0) 
  let hash_fold_t = Hash.hash2fold hash
  let pp fmt a =
    let rec aux fmt = function
      | [] -> ()
      | t::q -> fprintf fmt "%s%a" (if t then "1" else "0") aux q
    in
    aux fmt (List.map isT (CstBV.bits a))
  let show = Print.stringOf pp
  let name = "BV"
end
              
(* We are using the above as values *)
let vkey  = Values.Key.make(module V)
let vinj  = Value.inj vkey
let vproj = Value.proj vkey

(* We are using VarSets (sets of variables) as alternative term representations *)
module TS = Termstructures.VarSet.BV

module Arg = struct
  include Term
  include TypesFromHConsed(Term)
  include EmptyInfo
  type values = V.t
end

module VMap = struct
  include Map.MakeNH(Arg)
  let pp_pair fmt (term,v) = pp_sassign fmt (SAssign(term,Values.NonBoolean(vinj v)))
  let pp = print_in_fmt ~wrap:("{","}") pp_pair
end

module type API = sig
  type state
  val init: state
  val term_eval : (Term.t -> V.t) -> Term.t -> V.t
  val form_eval : (Term.t -> V.t) -> Term.t -> bool
  val eval : state -> Term.t -> (sign, straight) message
end

module T = struct
  let dskey = Termstructures.VarSet.Arrays.key
  let ds  = [DSK dskey]
  type nonrec sign = sign
  type api = (module API)
  let name = "BV"

  module Make(W : Writable) : API = struct

    let vinj  = Value.inj vkey
    let vproj = Value.proj vkey
    let proj  = Terms.proj dskey

    (* First, we implement evaluation functions for bitvector terms and predicates *)

    (* Datastructure that we use for the by-product of evaluation *)
    type 'a eval = { cst : 'a;                  (* by-product for a constant bitvector *)
                     sgl : Term.t -> V.t -> 'a; (* by-product for a variable with a value *)
                     uni : 'a -> 'a -> 'a }     (* by-product for a binary operation *)

    (* datastructure to use when we don't want any by-product *)
    let noby = { cst = ();
                 sgl = (fun _ _ -> ());
                 uni = fun () () -> () }

    (* datastructure to use when the by-product is the assignements used for evaluation *)
    let by = { cst = Assign.empty;
               sgl = (fun term v ->
                   (term, Values.NonBoolean(vinj v))
                   |> SAssign.build
                   |> Assign.singleton);
               uni = Assign.union }

    (* evaluation of a term *)
    let term_eval_by by valuation term =
      let rec aux term =
        match Term.reveal term with
        | Terms.C(Symbols.Extract{hi;lo},[a]) ->
          let a,a' = aux a in
          V.select_e a hi lo, a'
        | Terms.C(Symbols.Conc _,[a;b]) ->
          let a,a' = aux a in
          let b,b' = aux b in
          V.(a @: b), by.uni a' b'
        | Terms.C(Symbols.CstBV s,[]) ->
          V.constb s, by.cst
        | _ -> let v = valuation term in
          v, by.sgl term v
      in aux term

    (* Exception that we raise if we cannot evaluate a bitvector formula *)
    exception CannotEval of string

    (* evaluation of an atomic predicate *)
    let form_eval_by by valuation form =
      let term_eval = term_eval_by by valuation in
      match Term.reveal form with
      | Terms.C(Symbols.Eq(Sorts.BV _),[a;b]) ->
        let a,a' = term_eval a in
        let b,b' = term_eval b in
        V.equal a b, by.uni a' b'
      | Terms.C(Symbols.NEq(Sorts.BV _),[a;b]) ->
        let a,a' = term_eval a in
        let b,b' = term_eval b in
        not(V.equal a b), by.uni a' b'
      | _ -> raise(CannotEval(Print.stringOf Term.pp form))


    (* Evaluation without by-products *)
    let term_eval valuation = term_eval_by noby valuation >> fst
    let form_eval valuation = form_eval_by noby valuation >> fst

    (* States of the algorithm *)
    type state = { seen        : Assign.t;
                   valuation   : VMap.t;
                   constraints : Term.t list }

    (* Initial state *)
    let init = { seen        = Assign.empty;
                 valuation   = VMap.empty;
                 constraints = [] }

    (* Evaluation function for a formula, given the state of the algorithm.
       Either raises an exception CannotEval,
       or produces a message Propa(assign,boolassign), where
       boolassign is the Boolean assignment saying if the formula evaluates to true or false
       assign are the assignments that were used in the evaluation *)
    let eval state term =
      let f t = 
        if VMap.mem t state.valuation then VMap.find t state.valuation
        else raise (CannotEval(Term.show t))
      in
      match Term.get_sort term with
      | Sorts.Prop ->
        let b,assign = form_eval_by by f term
        in straight () assign (term,Values.Boolean b)
      (* | Sorts.BV _ -> *)
      (*    let v,assign = term_eval_by f by term *)
      (*    in Values.NonBoolean v, assign  *)
      | s -> raise (CannotEval(Format.toString(fun p ->
          p "I do not know sort %a" Sorts.pp s)))

  end

  let make (module W : Writable) : api = (module Make(W))

end

let hdl = register(module T)
