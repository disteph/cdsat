(*****************************)
(* The functor X => CC(X)    *)
(* producing a ground theory *)
(*****************************)

open Top
open Interfaces_basic
open Messages
open Specs

open Termstructures.Literals

open Interfaces
open CCX_algo

type sign = unit

module Make
         (DS: DSproj with type ts = LitF.t)
         (X : SolvableTheory with type VtoAssign.v = DS.Assign.t
                             and  type t = DS.Term.t
                             and  type v = DS.Term.t)
         (U : PersistentUnionFind with type e = X.v) = struct

  open DS
         
  module Alg = Algo(DS)(X)(U)

  let fromTerm a = 
    let b,t = LitF.reveal(proj(Terms.data a)) in
    let a' = Term.term_of_id t in
    match Terms.reveal a' with
    | Terms.C(Symbols.Eq so,[a1;a2]) when b -> Eq(so,a1,a2,Some(Term.id a))
    | Terms.C(Symbols.Eq so,[a1;a2])        -> NEq(so,a1,a2,Some(Term.id a))
    | Terms.C(Symbols.NEq so,[a1;a2]) when b-> NEq(so,a1,a2,Some(Term.id a))
    | Terms.C(Symbols.NEq so,[a1;a2])       -> Eq(so,a1,a2,Some(Term.id a))
    | _ -> match Term.get_sort a with
           | Sorts.Prop -> Eq(Sorts.Prop,
                              a',
                              Term.bC (if b then Symbols.True else Symbols.False) [],
                              Some(Term.id a))
           | _ -> assert false

  let fromAssign tset = 
    let tNeqf = NEq(Sorts.Prop, Term.bC Symbols.True [], Term.bC Symbols.False [],None) in
    tNeqf::(Assign.fold (fun t l -> (fromTerm t)::l) tset [])


  let toTerm = function
    | Eq(_,_,_,i) | NEq(_,_,_,i)
      -> i
    | Congr(_,_)  -> assert false

  let toAssign a = 
    List.fold (fun a e -> 
      match toTerm a with 
      | Some i -> Assign.add (Term.term_of_id i) e
      | None   -> e)
      a
      Assign.empty
      
  module type SlotMachineCC = sig
    type t
    val treated  : Assign.t
    val add      : Assign.t -> t
    val normalise: Term.t -> (sign,Assign.t,straight) message
  end

  type outputCC =
    | UNSAT of (sign, DS.Assign.t, unsat) message
    | SAT of
        (sign, DS.Assign.t, sat) message *
          (module SlotMachineCC with type t = outputCC)

  let rec machine treated s =
    (module struct

      type t = outputCC

      let treated = treated

      let add tset = 
        let newtreated = Assign.union treated tset in
        try 
          SAT(sat () newtreated, machine newtreated (Alg.algo s (fromAssign tset)))
        with
          Alg.Inconsistency l -> UNSAT(unsat () (toAssign l))

      let normalise t = 
        let t' = Alg.normalise s t in
        let so = Term.get_sort t in
        let l = Term.bC (Symbols.Eq so) [t;t'] in
        let justif = Alg.explain s.Alg.u t t' in
        straight () 
          (toAssign justif)
          (Assign.singleton l)

      let suicide _ = ()

    end: SlotMachineCC with type t = outputCC)

  let init = machine Assign.empty Alg.init

end
