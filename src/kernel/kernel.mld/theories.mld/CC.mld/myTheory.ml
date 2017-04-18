open Top
open Specs
open Messages

open Termstructures.Literals

type sign = CCX.sign

(* We are not using values *)
include Theory.HasNoValues

(* We are using LitF for term representation *)
type ts = LitF.t
let ts = Termstructures.Register.LitF

module type API = sig

  type termdata
  type value
  type assign

  module type SlotMachineCC = sig
    type t
    val treated : assign
    val add : assign -> t
    val normalise : termdata termF -> (sign, assign, straight) message
  end

  type outputCC =
    | UNSAT of (sign, assign, unsat) message
    | SAT of
        (sign, assign, sat) message *
          (module SlotMachineCC with type t = outputCC)

  val init : (module SlotMachineCC with type t = outputCC)

end

type ('t,'v,'a) api = (module API with type termdata = 't
                                   and type value    = 'v
                                   and type assign   = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module struct
     type termdata = t
     type value = v
     type assign = a
     include CCX.Make
      (DS)
      (EmptyCC.Make(DS))
      (MyPUF.Make(DS.Term))
   end)
