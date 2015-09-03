(*****************************)
(* The functor X => CC(X)    *)
(* producing a ground theory *)
(*****************************)

open Top
open Interfaces_basic
open Messages
open Specs

open Prop
open Literals

open Interfaces
open CCX_algo

type sign = unit

module Make
  (DS: sig 
    include GTheoryDSType
    val proj: Term.datatype -> LitF.t
  end)
  (X : SolvableTheory with type t = DS.Term.t)
  (U : PersistentUnionFind with type e = X.v 
                           and  type d = X.t input) 
  = 
struct

  open DS

  module Alg = Algo (X) (U)

  let get_eqNeq atomN =
    TSet.fold
      (fun e (lEq,lNEq)  ->
	let b,at = X.predicate e in
        match X.root at with
        | Some(Symbols.Eq _)  when b     -> e::lEq,lNEq
        | Some(Symbols.NEq _) when not b -> e::lEq,lNEq
        | Some(Symbols.NEq _) when b     -> lEq,e::lNEq
        | Some(Symbols.Eq _)  when not b -> lEq,e::lNEq
        | _ -> lEq,lNEq)
      atomN
      ([],[])

  let toTSet = 
    List.fold_left (fun e a -> TSet.add (X.itoA a) e) TSet.empty



  module type State = sig
    type t
    val add      : LitF.t -> t
    val normalise: Term.t -> Term.t*t
    val query    : X.t input -> bool*t
  end






  let goal_consistency atomN t =
    if TSet.mem t atomN then Some (TSet.add t TSet.empty)
    else
      begin
        Dump.msg (Some(fun p->p "Procedure called on goal %a under hypotheses\n%a" Term.print_in_fmt t TSet.print_in_fmt atomN)) None None;
        let lEq,lNEq = get_eqNeq atomN in
	let phi = List.map X.atoI lEq in
        let at  = X.itoA(X.atoI t) in
	let s   = X.directSubterms at in
        let result = 
          match X.root at with
          | Some(Symbols.Eq _) -> 
            begin
	      let q = Some [Alg.toQuery (X.atoI t)] in
	      match Alg.solve phi q with
	      | None   -> None
	      | Some l -> Some(toTSet l)
	        
            end
          | Some(Symbols.NEq _) ->
            List.fold_right
              (fun eq -> function
              | Some _ as a -> a
              | None ->
	        let s' = X.directSubterms eq in
	        let q  = Some(List.map2
                               (fun a a' -> Alg.toQuery (Eq(a,a')))
                               s s') in
                begin
	          match Alg.solve phi q with
                  | None   -> None
                  | Some l -> Some(TSet.add eq (toTSet l))
                end
              )
              lEq
              None
          | _ -> None
        in
        (match result with 
        | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
        | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" TSet.print_in_fmt x)) None None); 
        result
      end

  let consistency atomN = 
    Dump.msg (Some(fun p->p "Procedure called on\n%a" TSet.print_in_fmt atomN)) None None;
    let lEq,lNEq = get_eqNeq atomN in
    let phi = List.map X.atoI lEq in 
    let result = 
      match Alg.solve phi None with
      | Some l -> Some(toTSet l)
      | None   ->
        List.fold_right
          (fun neq -> function
          | Some _ as a -> a
          | None ->
            begin
	      match goal_consistency atomN (Term.bC Symbols.Neg [neq]) with
	      | None   -> None
	      | Some s -> Some(TSet.add neq s)
            end
          )
          lNEq
          None
    in
    (match result with 
    | None   -> Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None
    | Some x -> Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" TSet.print_in_fmt x)) None None); 
    result
end
