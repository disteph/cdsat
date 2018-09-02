open Basic
open Specs

module FVSubst = struct
  open Variables
  type t = FreeVar.t DSubst.t [@@deriving eq,show,hash]
  let get_arity = DSubst.get_arity
  let get = DSubst.get FreeVar.pp       
end
                   
let fail_state =
  let add _ = failwith "Are you dumb? I already told you it was provable" in
  let share   = add in
  let clone   = add in
  let suicide = add in
  let propose ?term i = add i in
  Specs.SlotMachine { add; propose; share; clone; suicide }

