open Interfaces_basic
open Specs

module FVSubst = struct
  open Variables
  type t = FreeVar.t DSubst.t [@@deriving eq,show,hash]
  let get_arity = DSubst.get_arity
  let get = DSubst.get FreeVar.pp       
end
                   
let fail_state =
  Specs.SlotMachine {
      add     = (fun _ -> failwith "Are you dumb? I already told you it was provable");
      clone   = (fun _ -> failwith "Are you dumb? I already told you it was provable");
      suicide = (fun _ -> failwith "Are you dumb? I already told you it was provable")
    }
                   
module Pairing(B1: DataType)(B2: DataType)
       : (DataType with type t = B1.t*B2.t) =
  struct
    type t = B1.t*B2.t
    let bC tag symb args = 
      (B1.bC tag symb (List.map fst args),
       B2.bC tag symb (List.map snd args))
    let bV tag v = (B1.bV tag v, B2.bV tag v)
    let bB tag t = (B1.bB tag t, B2.bB tag t)
  end
