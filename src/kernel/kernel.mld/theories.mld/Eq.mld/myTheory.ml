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

                    
module Make(DS: DSproj) = struct

  module E = Egraph.Make(DS)
  include E


  module P = struct
    module Node = TermValue
    type edge = sassign
    type nonrec info = info
  end

  module EG = Make(RawEgraph.Make(P))
    
  open DS

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
    val treated : Assign.t
    val add     : sassign -> t
  end

  type outputEq =
    | UNSAT of stop
    | SAT of
        (sign, Assign.t*boolassign, sat) message *
          (module SlotMachineEq with type t = outputEq)

                   
  type state = {
      egraph : EG.t;
      treated: Assign.t
    }

  let rec machine state =
    (module struct

       type t = outputEq

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
           (* Output(Some(sat () atomN), machine assign) *)
           SAT(sat () treated, machine { egraph = eg ; treated = treated })
         with
           EG.Conflict(propa,conflict) ->
           UNSAT(propa,conflict)
       (* Output(Some(unsat () tset), Tools.fail_state) *)

       let clone ()  = failwith "TODO" (* Output(None, machine assign) *)
       let suicide _ = ()

     end : SlotMachineEq with type t = outputEq)

  let init = machine { egraph = EG.init; treated = Assign.empty }

  let clear () = ()
                          
end


type ('t,'v,'a) api = (module API with type sign = sign
                                   and type termdata = 't
                                   and type value    = 'v
                                   and type assign   = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module struct
     type nonrec sign = sign
     type termdata = t
     type value = v
     type assign = a
     include Make(DS)
   end)