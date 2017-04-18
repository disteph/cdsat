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

  let rec state atomN =
    (module struct

      type newoutput = (sign,Assign.t) output
      type tset = Assign.t

      let treated () = atomN

      let add = function
        | None -> Output(None,state atomN)
        | Some tset ->
           let newtreated = Assign.union atomN tset in
           Output(Some(sat () newtreated),state newtreated)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, state atomN)

      let suicide _ = ()

    end : SlotMachine with type newoutput = (sign,Assign.t) output and type tset = Assign.t)

  let init = state Assign.empty
  let clear () = ()
                   
end

type ('t,'v,'a) api = (module API with type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
