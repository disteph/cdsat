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
    Specs.SlotMachine {
        add = (function
               | None -> Silence, state atomN
               | Some a ->
                  let newtreated = Assign.add a atomN in
                  Msg(sat () newtreated),state newtreated);
        
        clone   = (fun () -> state atomN);
        suicide = (fun _ -> ())
      }

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
