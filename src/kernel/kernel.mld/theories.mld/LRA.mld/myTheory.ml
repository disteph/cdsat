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
  type sassign = Term.t * Value.t Values.t
                                  
  let rec state atomN =
    (module struct

      type assign = Assign.t
      type sassign = Term.t * Value.t Values.t
      type newoutput = (sign,assign,sassign) output

      let treated () = atomN

      let add = function
        | None -> Output(None,state atomN)
        | Some a ->
           let newtreated = Assign.add a atomN in
           Output(Some(sat () newtreated),state newtreated)

      let normalise _ = failwith "Not a theory with normaliser"

      let clone () = Output(None, state atomN)

      let suicide _ = ()

     end : SlotMachine with type newoutput = (sign,Assign.t,Term.t * Value.t Values.t) output
                        and type assign = Assign.t
                        and type sassign = Term.t * Value.t Values.t)

  let init = state Assign.empty
  let clear () = ()
                   
end

type ('t,'v,'a) api = (module API with type assign = 'a
                                   and type sassign= ('t,'v)sassign)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
