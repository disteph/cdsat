open General
open Sums

open Top
open Basic
open Terms
open Values
open Sassigns
open Messages

open Theory

open Egraph
    
include MyTheory_sig

type sign = unit

(* As alternative term representation, we only use the free vars *)

let key = Termstructures.VarSet.Eq.key
let ds  = [DSK key]
type api = (module API with type sign = sign)
let name = "Eq"

module Make(W : Writable) = struct

  open W

  type nonrec sign = sign

  module EG = Make(W)

  module TVMap = Map.Make(TermValue)

  type state = { egraph : EG.t;
                 treated: Assign.t;
                 sharing : TSet.t;
                 myvars  : TSet.t Lazy.t }

  let add_myvars = Terms.proj key >> TSet.fold TSet.add

  let rec machine state =

    let add sassign ~level =
      Print.print ["kernel.egraph",1] (fun p->
          p "kernel.egraph adds %a" SAssign.pp sassign);
      let SAssign(term,value) = SAssign.reveal sassign in
      let myvars  = lazy(add_myvars term (Lazy.force state.myvars)) in
      let treated = Assign.add sassign state.treated in
      try
        Print.print ["kernel.egraph",1] (fun p-> p "kernel.egraph eq starting");
        let egraph,info,tvset =
          EG.eq term (Case2(Values value)) sassign ~level state.egraph
        in
        Print.print ["kernel.egraph",1] (fun p-> p "kernel.egraph eq finished");
        let tvmap = List.fold (fun x -> TVMap.add x info) tvset TVMap.empty in
        let aux t1 t2 value =
          let egraph,info,tvset = EG.eq t1 (Case1 t2) sassign ~level egraph in
          let tvmap = List.fold (fun x -> TVMap.add x info) tvset tvmap in
          egraph, tvmap
        in
        let egraph, tmap =
          match Term.reveal term, value with
          | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean true -> aux t1 t2 value
          | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean false -> aux t1 t2 value
          | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean true
            -> EG.diseq t1 t2 sassign ~level egraph, tvmap
          | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean false
            -> EG.diseq t1 t2 sassign ~level egraph, tvmap
          | _ -> egraph, tvmap
        in
        Print.print ["kernel.egraph",1] (fun p->
            p "kernel.egraph is fine with %a" Assign.pp treated);
        SAT(sat () treated ~sharing:state.sharing ~myvars,
            machine { state with egraph; treated; myvars })
      with
        Conflict(propa,conflict) ->
        Print.print ["kernel.egraph",0] (fun p->
            p "kernel.egraph detected conflict:\n %a\n leads to %a"
              (List.pp pp_imessage) propa
              pp_imessage conflict);
        UNSAT(propa,conflict)
    in

    let share tset =
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      SAT(sat () state.treated ~sharing ~myvars,
          machine { state with sharing; myvars })
    in

    let watchfind key ~howmany tset =
      let egraph,watched = EG.watchfind key howmany tset state.egraph in
      watched, machine { state with egraph }
    in
    
    let ask =
      let ask ?subscribe tv =
        let info,egraph = EG.ask ?subscribe tv state.egraph in
        EG.nf info,
        EG.cval info,
        (fun () -> EG.distinct egraph info),
        machine { state with egraph }
      in ask

    in { add; share; watchfind; ask }

  let init = machine { egraph  = EG.init;
                       treated = Assign.empty;
                       sharing = TSet.empty;
                       myvars  = lazy TSet.empty }

end

let make (module W : Writable) : api = (module Make(W))
