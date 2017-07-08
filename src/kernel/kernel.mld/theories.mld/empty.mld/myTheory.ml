open Top
open Messages
open Specs

type sign = unit

(* We are not using values *)
include Theory.HasNoValues

(* We are not using alternative term representation *)
type ts = unit
let ts = Termstructures.Register.NoRep

module Make(DS: GlobalDS) = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t
  type sassign = Term.t * Value.t Values.t [@@deriving eq, show]

  let rec machine atomN =
    SlotMachine(
        module struct

          type term   = Term.t
          type value  = Value.t
          type assign = Assign.t
          type newoutput = (sign,Term.datatype * Value.t * Assign.t) output

          let add = function
            | None -> Output(None, machine atomN)
            | Some a ->
               Dump.print ["empty",1] (fun p-> p "Empty adds %a" pp_sassign a);
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

        end : SlotMachine  with type newoutput = (sign,Term.datatype * Value.t * Assign.t) output
                            and type term   = Term.t
                            and type value  = Value.t
                            and type assign = Assign.t)

  let init = machine Assign.empty
  let clear () = ()
                   
end

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a) api = (module API with type datatypes = 't*'v*'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
