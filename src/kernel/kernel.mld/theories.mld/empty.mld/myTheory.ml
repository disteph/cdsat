open Top
open Messages
open Specs

type sign = unit

(* We are not using values *)
include Theory.HasNoValues

(* We are not using alternative term representation *)
type ts = unit
let ts = Termstructures.Register.NoRep

module type API = sig
  type assign
  val init: (sign,assign) slot_machine
  val clear: unit -> unit
end

module Make(DS: GTheoryDSType) = struct

  type assign = DS.Assign.t
  open DS

  let rec machine atomN
      = (module struct

        type newoutput = (sign,Assign.t) output
        type tset = Assign.t

        let add = function
          | None -> Output(None, machine atomN)
          | Some newtset ->
             Dump.print ["empty",1] (fun p-> p "Empty adds %a" Assign.pp newtset);
             let atomN = Assign.union newtset atomN in
             let tmp = Assign.fold
               (fun l -> function
               | Some _ as ans -> ans
               | None ->
                  let notl = Term.bC Symbols.Neg [l] in
                  if Assign.mem notl atomN 
                  then Some(Assign.add l (Assign.add notl Assign.empty))
                  else None
               )
               atomN
               None in
             match tmp with
             | Some tset -> Output(Some(unsat () tset), Tools.fail_state)
             | None ->
                Dump.print ["empty",1] (fun p-> p "Empty is fine with %a" Assign.pp atomN);
                Output(Some(sat () atomN), machine atomN)

        let normalise _ = failwith "Not a theory with normaliser"

        let clone () = Output(None, machine atomN)

      let suicide _ = ()

      end : SlotMachine with type newoutput = (sign,Assign.t) output and type tset = Assign.t)

  let init = machine Assign.empty
  let clear () = ()
                   
end

type ('t,'v,'a) api = (module API with type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
