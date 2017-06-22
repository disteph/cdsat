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
  type sassign
  val init: (sign,assign,sassign) slot_machine
  val clear: unit -> unit
end

module Make(DS: GlobalDS) = struct

  open DS
  type assign = Assign.t
  module SAssign = Tools.SAssign(DS)
  type sassign = SAssign.t

  let rec machine atomN
      = (module struct

        type newoutput = (sign,Assign.t,SAssign.t) output
        type nonrec assign = assign
        type nonrec sassign = sassign

        let add = function
          | None -> Output(None, machine atomN)
          | Some a ->
             Dump.print ["empty",1] (fun p-> p "Empty adds %a" SAssign.pp a);
             let atomN = Assign.add a atomN in
             let tmp =
               Assign.fold
                 (fun (l,v) sofar ->
                   match v,sofar with
                   | _, (Some _ as ans) -> ans
                   | Values.NonBoolean _, ans -> ans
                   | Values.Boolean b, None ->
                      let notl = l, Values.Boolean(not b) in
                      if Assign.mem notl atomN 
                      then Some(Assign.add (l,v) (Assign.add notl Assign.empty))
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

         end : SlotMachine with type newoutput = (sign,assign,sassign) output
                            and type assign = assign
                            and type sassign = sassign)

  let init = machine Assign.empty
  let clear () = ()
                   
end

type ('t,'v,'a) api = (module API with type assign = 'a
                                   and type sassign = ('t,'v)sassign)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
