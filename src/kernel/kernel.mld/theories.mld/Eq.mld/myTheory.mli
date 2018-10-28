open Top.Terms

open Theory

include module type of MyTheory_sig
    
val ds : dsKey list
type sign
type api = (module API with type sign = sign)
val make : (module Writable) -> api
val name : string
