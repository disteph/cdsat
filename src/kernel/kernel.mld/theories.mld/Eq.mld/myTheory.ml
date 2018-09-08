open General
open Sums

open Top
open Basic
open Terms
open Values
open Sassigns
open Messages

open Theory
open Interfaces
       
type sign = unit

(* As alternative term representation, we only use the free vars *)
                    
module T = struct
  let key = Termstructures.VarSet.Eq.key
  let ds  = [DSK key]
  type nonrec sign = sign
  type api = (module API)
  let name = "Eq"

  module Make(W : Writable) : API = struct

    open W

    include Egraph.Make(W)

    module TMap = struct
      include Map.Make(Term)
      let pp x fmt tmap =
        let aux fmt (nf,j) = Format.fprintf fmt "(%a->%a)" Term.pp nf x j in
        List.pp aux fmt (bindings tmap)
    end

    module TVMap = Map.Make(TermValue)

    module TVSet = struct
      include Set.Make(TermValue)
      let pp fmt tvset = List.pp TermValue.pp fmt (elements tvset)
    end                

    type output =
      | UNSAT of stop
      | SAT of (sign, sat) message * self

    and self = { add : sassign -> output;
                 share : TSet.t  -> output;
                 ask : ?subscribe:bool
                   -> (Term.t, Value.t values) sum
                   -> Term.t
                      * CValue.t
                      * (unit -> CValue.t list)
                      * self }

    type state = { egraph : EG.t;
                   treated: Assign.t;
                   sharing : TSet.t;
                   myvars  : TSet.t Lazy.t }

    let add_myvars = Terms.proj key >> TSet.fold TSet.add

    let rec machine state =

      let add sassign =       
        Print.print ["kernel.egraph",1] (fun p->
            p "kernel.egraph adds %a" SAssign.pp sassign);
        let SAssign(term,value) = SAssign.reveal sassign in
        let myvars  = lazy(add_myvars term (Lazy.force state.myvars)) in
        let treated = Assign.add sassign state.treated in
        try
          Print.print ["kernel.egraph",1] (fun p-> p "kernel.egraph eq starting");
          let egraph,info,tvset =
            EG.eq term (Case2(Values value)) (Case2 sassign) state.egraph
          in
          Print.print ["kernel.egraph",1] (fun p->
              p "kernel.egraph eq finished");
          let tvmap = List.fold (fun x -> TVMap.add x info) tvset TVMap.empty in
          let aux t1 t2 value =
            let egraph,info,tvset = EG.eq t1 (Case1 t2) (Case1(term,value)) egraph in
            let tvmap = List.fold (fun x -> TVMap.add x info) tvset tvmap in
            egraph, tvmap
          in
          let egraph, tvmap =
            match Terms.reveal term, value with
            | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean true -> aux t1 t2 value
            | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean false -> aux t1 t2 value
            | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean true
              -> EG.diseq t1 t2 (term,value) egraph, tvmap
            | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean false
              -> EG.diseq t1 t2 (term,value) egraph, tvmap
            | _ -> egraph, tvmap
          in
          Print.print ["kernel.egraph",1] (fun p->
              p "kernel.egraph is fine with %a" Assign.pp treated);
          SAT(sat () treated ~sharing:state.sharing ~myvars,
              machine { state with egraph; treated; myvars })
        with
          EG.Conflict(propa,conflict) ->
          Print.print ["kernel.egraph",0] (fun p->
              p "kernel.egraph detected conflict:\n %a\n leads to %a"
                (List.pp Msg.pp) propa
                Msg.pp conflict);
          UNSAT(propa,conflict)
      in

      let share tset =
        let sharing = TSet.union tset state.sharing in
        let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
        SAT(sat () state.treated ~sharing ~myvars,
            machine { state with sharing; myvars })
      in

      let ask =
        let ask ?subscribe tv =
          let info,egraph = EG.ask ?subscribe tv state.egraph in
          EG.nf info,
          EG.cval info,
          (fun () -> EG.distinct egraph info),
          machine { state with egraph }
        in ask

      in { add; share; ask }

    let init = machine { egraph  = EG.init;
                         treated = Assign.empty;
                         sharing = TSet.empty;
                         myvars  = lazy TSet.empty }

  end
end
