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
                                  
  let rec state atomN =
    SlotMachine(
        module struct
          
          type term   = Term.t
          type value  = Value.t
          type assign = Assign.t
          type newoutput = (sign,Term.datatype * Value.t * Assign.t) output

          let treated () = atomN

          let add = function
            | None -> Output(None,state atomN)
            | Some a ->
               let newtreated = Assign.add a atomN in
               Output(Some(sat () newtreated),state newtreated)

          let normalise _ = failwith "Not a theory with normaliser"

          let clone () = Output(None, state atomN)

          let suicide _ = ()

        end : SlotMachine with type newoutput = (sign,Term.datatype * Value.t * Assign.t) output
                           and type term   = Term.t
                           and type value  = Value.t
                           and type assign = Assign.t)

  let init = state Assign.empty
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
