include module type of Termstructure_sig

module Pairing(B1: Type)(B2: Type) : (Type with type t = B1.t*B2.t)
