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
  (U : PersistentUnionFind with type e = X.v 
                           and  type d = DS.Term.t input) 
  = 
struct

  open DS

  module Alg = Algo (DS) (X) (U)

  let atoI a = 
    let b,t = LitF.reveal(proj(Terms.data a)) in
    let a = Term.term_of_id t in
    match Terms.reveal a with
    | Terms.C(Symbols.Eq so,[a1;a2]) when b -> Eq(so,a1,a2)
    | Terms.C(Symbols.Eq so,[a1;a2])        -> NEq(so,a1,a2)
    | Terms.C(Symbols.NEq so,[a1;a2]) when b-> NEq(so,a1,a2)
    | Terms.C(Symbols.NEq so,[a1;a2])       -> Eq(so,a1,a2)
    | _ when get_sort a = Sorts.Prop -> Eq(Sorts.Prop, a, Term.bC (if b then Symbols.True else Symbols.False) [])
    | _ -> assert false

  let itoA = function
    | Eq(so,a,b)  -> Term.bC (Symbols.Eq so) [a;b]
    | NEq(so,a,b) -> Term.bC (Symbols.NEq so) [a;b]
    | Congr(_,_)  -> assert false

  let toTSet = 
    List.fold_left (fun e a -> TSet.add (itoA a) e) TSet.empty

  let fromTSet tset = 
    let tNeqf = NEq(Sorts.Prop, Term.bC Symbols.True [], Term.bC Symbols.False []) in
    tNeqf::(TSet.fold (fun t l -> (atoI t)::l) tset [])

  module type State = sig
    type t
    val treated  : TSet.t
    val add      : TSet.t -> t
    val normalise: Term.t -> (sign,TSet.t,thStraight) thsays
  end

  type state = | UNSAT of (sign,TSet.t,thProvable) thsays
               | SAT of (sign,TSet.t,thNotProvable) thsays * (module State with type t = state)

  let rec getModule treated s =
    (module struct

      type t = state

      let treated = treated

      let add tset = 
        U.clear ();
        let newtreated = TSet.union treated tset in
        try 
          SAT(thNotProvable () newtreated, getModule newtreated (Alg.algo s (fromTSet tset)))
        with
          Alg.Inconsistency l -> UNSAT(thProvable () (toTSet l))

      let normalise t = 
        let t' = Alg.normalise s t in
        let so = get_sort t in
        let l = itoA(Eq(so,t,t')) in
        let justif = Alg.explain s.Alg.u t t' in
        thStraight () 
          (TSet.add l TSet.empty) 
          (List.fold_left (fun e a -> TSet.add (itoA a) e) TSet.empty justif)

    end: State with type t = state)

  let init = getModule TSet.empty Alg.init

end
