open Top.Terms
       
include Termstructure_sig

module Pairing(B1: Type)(B2: Type) = struct
  type t = B1.t*B2.t

  let key = ThTermKey.create_key(module struct type nonrec t = t let name = "pair" end)
  let build proj term =
    (B1.build (proj >> fst) term,
     B2.build (proj >> snd) term)
end
