open General
open Sums

open Top
open Specs
open Sassigns
open Messages

open Interfaces
       
type sign = unit

(* The EGraph doesn't add its own kind of values, it uses the other theories' *)
include Theory.HasNoValues

(* The EGraph does not need alterative term representations *)
type ts = unit
let ts = Termstructures.Register.NoRep

                    
module Make(DS: GlobalDS) = struct

  open DS
  include Egraph.Make(DS)

  module P = struct
    module Node = TermValue
    type edge = (bassign,sassign)sum [@@deriving show]
    type nonrec info = info
  end

  module EG = Make(RawEgraph.Make(P))
    
  type nonrec sign = sign
  type termdata = Term.datatype
  type value  = Value.t
  type assign = Assign.t
  type cval   = CValue.t
  type sassign = DS.sassign
  type bassign = DS.bassign

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
    | SAT of (sign, assign * bassign, sat) message * self

   and self = {
       treated : assign;
       add     : sassign -> output;
       ask     : ?subscribe:bool
                 -> (termdata termF, value Sassigns.values) sum
                 -> (termdata termF)
                    * cval
                    * (unit -> cval list)
                    * self
     }
                   
  type state = {
      egraph : EG.t;
      treated: Assign.t
    }

  let rec machine state = {

      treated = state.treated;
                  
      add =
        (function
           sassign ->
           Print.print ["kernel.egraph",1] (fun p-> p "kernel.egraph adds %a" pp_sassign sassign);
           let SAssign(term,value) = sassign in
           let treated = Assign.add sassign state.treated in
           try
             let eg,info,tvset = EG.eq term (Case2(Values value)) (Case2 sassign) state.egraph in
             let tvmap = List.fold (fun x -> TVMap.add x info) tvset TVMap.empty in
             let aux t1 t2 value =
               let eg,info,tvset = EG.eq t1 (Case1 t2) (Case1(term,value)) eg in
               let tvmap = List.fold (fun x -> TVMap.add x info) tvset tvmap in
               eg, tvmap
             in
             let eg, tvmap =
               match Terms.reveal term, value with
               | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean true -> aux t1 t2 value
               | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean false -> aux t1 t2 value
               | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean true
                 -> EG.diseq t1 t2 (term,value) eg, tvmap
               | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean false
                 -> EG.diseq t1 t2 (term,value) eg, tvmap
               | _ -> eg, tvmap
             in
             Print.print ["kernel.egraph",1] (fun p-> p "kernel.egraph is fine with %a" Assign.pp treated);
             SAT(sat () treated, machine { egraph = eg ; treated = treated })
           with
             EG.Conflict(propa,conflict) ->
             Print.print ["kernel.egraph",1] (fun p->
                 p "kernel.egraph detected conflict:\n %a\n leads to %a"
                   (List.pp Msg.pp) propa
                   Msg.pp conflict);
             UNSAT(propa,conflict)
        );

      ask =
        let ask ?subscribe tv =
          let info,eg = EG.ask ?subscribe tv state.egraph in
          EG.nf info,
          EG.cval info,
          (fun () -> EG.distinct eg info),
          machine { state with egraph = eg }
        in ask
    }

  let init = machine { egraph = EG.init; treated = Assign.empty }

end
