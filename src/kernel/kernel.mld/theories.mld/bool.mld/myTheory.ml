open General

open Top
open Messages
open Specs

type sign = unit

(* We are not using values *)
include Theory.HasNoValues

(* We are not using alternative term representation *)
type ts = Termstructures.Clauses.TS.t
let ts = Termstructures.Register.Clauses

module Make(DS: GlobalDS) = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t

  let rec machine state =
    Specs.SlotMachine {
        add =
          (function
           | None ->
              Print.print ["kernel.bool",2] (fun p ->
                  p "kernel.bool receiving None");
              Silence, machine state
           | Some a ->
              Print.print ["kernel.bool",2] (fun p ->
                  p "kernel.bool receiving Some(%a)"
                    pp_sassign a
                );
              let state = Assign.add a state in
              let term,v = a in
              match Terms.reveal term, v with
              | Terms.C(Symbols.Neg,[t]), Values.Boolean b
                   when not(Assign.mem (t,Values.Boolean(not b)) state)
                ->
                 let assign = Assign.singleton a in
                 let msg = straight () assign (t,not b) in
                 Print.print ["kernel.bool",2] (fun p ->
                     p "kernel.bool propagating %a" Msg.pp msg);
                 Msg msg, machine state
              | _ ->               
                 Msg(sat () state), machine state);
        
        clone   = (fun () -> machine state);
        suicide = (fun _ -> ())
      }

  let init = machine Assign.empty
  let clear () = ()
                   
end

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a) api = (module API with type datatypes = 't*'v*'a)

let make (type t v a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))
