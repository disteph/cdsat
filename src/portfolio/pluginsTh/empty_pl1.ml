open Kernel
open Top
open Specs
open Messages
open Empty
open PluginTh

module Make(DS: GTheoryDSType) = struct 

  type sign = Mytheory.sign
  type tset = DS.TSet.t
  type slot_machine = SM:
    (sign,tset,'msg) thsays option
    *(tset eat_this option -> slot_machine)
    -> slot_machine

  module MyEmpty = Mytheory.Make(DS)

  let rec search tset = match MyEmpty.consistency tset with
    | MyEmpty.L(msg) -> SM(Some msg, fun _ -> failwith "Are you dumb? I already told you it was provable")
    | MyEmpty.R(msg) -> SM(Some msg, function
      | None -> search tset
      | Some (EatThis(ThStraight(newtset,f))) -> search (DS.TSet.union newtset tset))

end
