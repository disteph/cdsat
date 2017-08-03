open General
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
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t

  type state = { assign : Assign.t;
                 sharing: TSet.t;
                 myvars : TSet.t }
                 
  let rec machine state =
    let add = function
      | None ->
         Print.print ["kernel.arrays",2] (fun p ->
             p "kernel.arrays receiving None");
         Silence, machine state
      | Some a ->
         Print.print ["kernel.arrays",2] (fun p ->
             p "kernel.arrays receiving Some(%a)" pp_sassign a);
         let state = { state with assign = Assign.add a state.assign } in
         Msg(sat () state.assign ~sharing:state.sharing ~myvars:state.myvars ),
         machine state
    in
    let share tset = 
      Print.print ["kernel.arrays",2] (fun p ->
          p "kernel.arrays notified than %a are shared" TSet.pp tset);
      Silence, machine state
    in
    let clone () = machine state in
    let suicide _ = () in
    Specs.SlotMachine { add; share; clone; suicide }

  let init = machine { assign=Assign.empty; sharing=TSet.empty; myvars=TSet.empty }
  let clear () = ()
                   
end

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a,'s) api = (module API with type datatypes = 't*'v*'a*'s)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))
