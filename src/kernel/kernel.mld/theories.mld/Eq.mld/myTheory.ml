open General.Sums

open Top
open Specs
open Messages

open Interfaces
       
type sign = unit

(* The EGraph doesn't add its own kind of values, it uses the other theories' *)
include Theory.HasNoValues

(* The EGraph does not need alterative term representations *)
type ts = unit
let ts = Termstructures.Register.NoRep

                    
module Make(DS: GlobalDS) = struct

  include Egraph.Make(DS)

  module P = struct
    module Node = TermValue
    type edge = sassign
    type nonrec info = info
  end

  module EG = Make(RawEgraph.Make(P))
    
  open DS
  type nonrec sign = sign
  type termdata = Term.datatype
  type value  = Value.t
  type assign = Assign.t
  type cval   = CValue.t

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

  module type SlotMachineEq = sig
    type t
    type self
    val treated : Assign.t
    val add     : sassign -> t
    val ask     : ?subscribe:bool
                  -> (Term.t,Value.t Values.t) sum
                  -> Term.t
                     * CValue.t
                     * (unit -> CValue.t list)
                     * self
  end

  type outputEq =
    | UNSAT of stop
    | SAT of (sign, Assign.t * boolassign, sat) message
             * (module SlotMachineEq with type t = outputEq
                                      and type self = self)
   and self = Self of (module SlotMachineEq with type t = outputEq
                                             and type self = self)
                   
  type state = {
      egraph : EG.t;
      treated: Assign.t
    }

  let rec machine state =
    (module struct

       type t = outputEq
       type nonrec self = self

       let treated = state.treated
                  
       let add sassign =
         Dump.print ["egraph",1] (fun p-> p "EGraph adds %a" pp_sassign sassign);
         let term,value = sassign in
         let treated = Assign.add sassign state.treated in
         try
           let eg,info,tvset = EG.eq term (Case2 value) sassign state.egraph in
           let tvmap = List.fold (fun x -> TVMap.add x info) tvset TVMap.empty in
           let eg, tvmap =
             match Terms.reveal term, value with
             | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean true
               | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean false
               -> let eg,info,tvset = EG.eq t1 (Case1 t2) sassign eg in
                  let tvmap = List.fold (fun x -> TVMap.add x info) tvset tvmap in
                  eg, tvmap
             | Terms.C(Symbols.NEq s,[t1;t2]), Values.Boolean true
               -> EG.diseq t1 t2 (term,true) eg, tvmap
             | Terms.C(Symbols.Eq s,[t1;t2]), Values.Boolean false
               -> EG.diseq t1 t2 (term,false) eg, tvmap
             | _ -> eg, tvmap
           in
           Dump.print ["egraph",1] (fun p-> p "EGraph is fine with %a" Assign.pp treated);
           SAT(sat () treated, machine { egraph = eg ; treated = treated })
         with
           EG.Conflict(propa,conflict) ->
           UNSAT(propa,conflict)

       let ask ?subscribe tv =
         let info,eg = EG.ask ?subscribe tv state.egraph in
         EG.nf info,
         EG.cval info,
         (fun () -> EG.distinct eg info),
         Self(machine { state with egraph = eg })
             
     end : SlotMachineEq with type t = outputEq
                          and type self = self)

  let init = machine { egraph = EG.init; treated = Assign.empty }

  let clear () = ()

end


(* type ('t,'v,'a) api = (module API with type sign = sign *)
(*                                    and type termdata = 't *)
(*                                    and type value    = 'v *)
(*                                    and type assign   = 'a) *)

(* let make (type t)(type v)(type a) *)
(*       ((module DS): (ts,values,t,v,a) dsProj) *)
(*     : (t,v,a) api = *)
(*   (module Make(DS)) *)
