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
  (X : SolvableTheory with type VtoTSet.v = DS.TSet.t
                      and  type t = DS.Term.t
                      and  type v = DS.Term.t)
  (U : PersistentUnionFind with type e = X.v) = 
struct

  open DS

  module Alg = Algo (DS) (X) (U)

  let fromTerm a = 
    let b,t = LitF.reveal(proj(Terms.data a)) in
    let a' = Term.term_of_id t in
    match Terms.reveal a' with
    | Terms.C(Symbols.Eq so,[a1;a2]) when b -> Eq(so,a1,a2,Some(Terms.id a))
    | Terms.C(Symbols.Eq so,[a1;a2])        -> NEq(so,a1,a2,Some(Terms.id a))
    | Terms.C(Symbols.NEq so,[a1;a2]) when b-> NEq(so,a1,a2,Some(Terms.id a))
    | Terms.C(Symbols.NEq so,[a1;a2])       -> Eq(so,a1,a2,Some(Terms.id a))
    | _ -> match get_sort a with
           | Sorts.Prop -> Eq(Sorts.Prop,
                              a',
                              Term.bC (if b then Symbols.True else Symbols.False) [],
                              Some(Terms.id a))
           | _ -> assert false

  let fromTSet tset = 
    let tNeqf = NEq(Sorts.Prop, Term.bC Symbols.True [], Term.bC Symbols.False [],None) in
    tNeqf::(TSet.fold (fun t l -> (fromTerm t)::l) tset [])


  let toTerm = function
    | Eq(_,_,_,i) | NEq(_,_,_,i)
      -> i
    | Congr(_,_)  -> assert false

  let toTSet a = 
    List.fold (fun a e -> 
      match toTerm a with 
      | Some i -> TSet.add (Term.term_of_id i) e
      | None   -> e)
      a
      TSet.empty
      
  module type SlotMachineCC = sig
    type t
    val treated  : TSet.t
    val add      : TSet.t -> t
    val normalise: Term.t -> (sign,TSet.t,straight) message
  end

  type outputCC =
    | UNSAT of (sign, DS.TSet.t, unsat) message
    | SAT of
        (sign, DS.TSet.t, sat) message *
          (module SlotMachineCC with type t = outputCC)

  let rec machine treated s =
    (module struct

      type t = outputCC

      let treated = treated

      let add tset = 
        let newtreated = TSet.union treated tset in
        try 
          SAT(sat () newtreated, machine newtreated (Alg.algo s (fromTSet tset)))
        with
          Alg.Inconsistency l -> UNSAT(unsat () (toTSet l))

      let normalise t = 
        let t' = Alg.normalise s t in
        let so = get_sort t in
        let l = Term.bC (Symbols.Eq so) [t;t'] in
        let justif = Alg.explain s.Alg.u t t' in
        straight () 
          (toTSet justif)
          (TSet.singleton l)

      let suicide _ = ()

    end: SlotMachineCC with type t = outputCC)

  let init = machine TSet.empty Alg.init

end
