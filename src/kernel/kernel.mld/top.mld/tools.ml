open Specs

let get_sort t = match Terms.reveal t with
  | Terms.V fv      -> Variables.FreeVar.get_sort fv
  | Terms.C(symb,_) -> let so,_ = Symbols.arity symb in so

let fail_state (type sign) (type ts) : (sign,ts) Specs.slot_machine =
  (module struct 
     type newoutput = (sign,ts) Specs.output
     type tset = ts
     let treated _ = failwith "Are you dumb? I already told you it was provable"
     let add     _ = failwith "Are you dumb? I already told you it was provable"
     let normalise _ = failwith "Are you dumb? I already told you it was provable"
     let clone   _ = failwith "Are you dumb? I already told you it was provable"
     let suicide _ = failwith "Are you dumb? I already told you it was provable"
   end)

module Pairing(B1: DataType)(B2: DataType)
       : (DataType with type t = B1.t*B2.t) =
  struct
    type t = B1.t*B2.t
    let bC tag symb args = 
      (B1.bC tag symb (List.map fst args),
       B2.bC tag symb (List.map snd args))
    let bV tag v = (B1.bV tag v, B2.bV tag v)
  end
