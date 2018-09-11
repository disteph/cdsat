open Top.Terms

module Pairing(B1: ThTerm)(B2: ThTerm) : (ThTerm with type t = B1.t*B2.t)
