open Top.Specs

type sign

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

include Theory.Type
        with type ('t,'v,'a,'s) api = (module API with type datatypes = 't*'v*'a*'s)
                        
