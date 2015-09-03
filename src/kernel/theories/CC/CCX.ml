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

  let toTSet = 
    List.fold_left (fun e a -> TSet.add (X.itoA a) e) TSet.empty

  let fromTSet tset = 
    TSet.fold 
      (fun t l -> match X.atoI t with 
      | None -> l
      | Some e -> e::l)
      tset
      []

  module type State = sig
    type t
    val add      : TSet.t -> t
    val normalise: Term.t -> Term.t
  end

  type state = | UNSAT of TSet.t
               | SAT of (module State with type t = state)

  let rec getModule s =
    (module struct

      type t = state

      let add tset = 
        try 
          SAT(getModule(Alg.algo {s with Alg.phi = fromTSet tset}))
        with
          Alg.Inconsistency l -> UNSAT(toTSet l)

      let normalise t = failwith "TODO" (* Alg.normalise s t *)

    end: State with type t = state)

  let init = getModule Alg.init

  let consistency atomN = 
    Dump.msg (Some(fun p->p "Procedure called on\n%a" TSet.print_in_fmt atomN)) None None;
    let module Init = (val init) in
    match Init.add atomN with
    | UNSAT l -> 
      Dump.msg (Some(fun p->p "Procedure finished with INCONSISTENT with hypotheses\n %a" TSet.print_in_fmt l)) None None; 
      Some l
    | SAT _ -> 
      Dump.msg (Some(fun p->p "Procedure finished with CONSISTENT with hypotheses.")) None None;
      None
end
