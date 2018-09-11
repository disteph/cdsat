open Top.Terms
       
module Pairing(B1: ThTerm)(B2: ThTerm) = struct
  type t = B1.t*B2.t
  let build ~reccall term = (B1.build ~reccall:(reccall >> fst) term,
                             B2.build ~reccall:(reccall >> snd) term)
  let name = "Pair("^B1.name^","^B2.name
end
