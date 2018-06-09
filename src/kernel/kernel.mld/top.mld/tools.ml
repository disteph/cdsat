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
                   
module Pairing(B1: DataType)(B2: DataType)
       : (DataType with type t = B1.t*B2.t) =
  struct
    type t = B1.t*B2.t
    let build proj term =
      let proj1 data = data |> proj |> fst in
      let proj2 data = data |> proj |> snd in
      (B1.build proj1 term, B2.build proj2 term)
  end
