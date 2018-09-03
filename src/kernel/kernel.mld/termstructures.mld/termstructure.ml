open Top.Terms
       
include Termstructure_sig

module Pairing(B1: Type)(B2: Type) = struct
  type t = B1.t*B2.t

  let key = ThTermKey.make(module struct type nonrec t = t let name = "pair" end)
  let build term = (B1.build term, B2.build term)
end
