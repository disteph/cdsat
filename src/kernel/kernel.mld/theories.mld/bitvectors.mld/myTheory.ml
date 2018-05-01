open Format
       
open General
open Top
open Basic
open Messages
open Specs
open Sassigns

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
end
              
(* We are using the above as values *)
include Theory.HasValues(V)

(* We are using VarSets (sets of variables) as alternative term representations *)
module TS = Termstructures.VarSet.BV

module type API = sig
  type termdata
  type value
  type assign
  type tset
  type state
  val init: state
  val term_eval : (int -> V.t) -> termdata termF -> V.t
  val form_eval : (int -> V.t) -> termdata termF -> bool
  val eval : state -> termdata termF
             -> (sign, assign * (termdata termF, _)bassign*tset, straight) message
  val vinj : V.t -> value
  val vproj : value -> V.t option
  val proj : termdata termF -> tset
end

type ('t,'v,'a,'s) api = (module API with type termdata = 't
                                      and type value  = 'v
                                      and type assign = 'a
                                      and type tset   = 's)

let make (type t v a s)
    ((module DS): ((t,s)TS.t,values,t,v,a,s) dsProj)
  : (t,v,a,s) api =
  (module struct

    open DS
    type termdata = Term.datatype
    type value = Value.t
    type assign = Assign.t
    type tset = TSet.t

    let HasVconv{ vinj; vproj } = conv
    let proj t = Terms.data t |> proj

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
                   Assign.singleton(SAssign(term, Values.NonBoolean(vinj v))));
               uni = Assign.union }

    (* evaluation of a term *)
    let term_eval_by valuation by term =
      let rec aux term =
        match Terms.reveal term with
        | Terms.C(Symbols.Extract{hi;lo},[a]) ->
          let a,a' = aux a in
          V.select_e a hi lo, a'
        | Terms.C(Symbols.Conc _,[a;b]) ->
          let a,a' = aux a in
          let b,b' = aux b in
          V.(a @: b), by.uni a' b'
        | Terms.C(Symbols.CstBV s,[]) ->
          V.constb s, by.cst
        | _ -> let v = valuation(Term.id term) in
          v, by.sgl term v
      in aux term

    (* Exception that we raise if we cannot evaluate a bitvector formula *)
    exception CannotEval of string

    (* evaluation of an atomic predicate *)
    let form_eval_by valuation by form =
      let term_eval = term_eval_by valuation by in
      match Terms.reveal form with
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
    let term_eval valuation term = fst(term_eval_by valuation noby term)
    let form_eval valuation term = fst(form_eval_by valuation noby term)

    (* States of the algorithm *)
    type state = { seen        : Assign.t;
                   valuation   : V.t IntMap.t;
                   constraints : Term.t list }

    (* Initial state *)
    let init = { seen        = Assign.empty;
                 valuation   = IntMap.empty;
                 constraints = [] }

    (* Evaluation function for a formula, given the state of the algorithm.
       Either raises an exception CannotEval,
       or produces a message Propa(assign,boolassign), where
       boolassign is the Boolean assignment saying if the formula evaluates to true or false
       assign are the assignments that were used in the evaluation *)
    let eval state term =
      let f i = 
        if IntMap.mem i state.valuation then IntMap.find i state.valuation
        else raise (CannotEval(string_of_int i))
      in
      match Term.get_sort term with
      | Sorts.Prop ->
        let b,assign = form_eval_by f by term
        in straight () assign (term,Values.Boolean b)
      (* | Sorts.BV _ -> *)
      (*    let v,assign = term_eval_by f by term *)
      (*    in Values.NonBoolean v, assign  *)
      | s -> raise (CannotEval(Print.toString(fun p ->
          p "I do not know sort %a" Sorts.pp s)))

  end)

