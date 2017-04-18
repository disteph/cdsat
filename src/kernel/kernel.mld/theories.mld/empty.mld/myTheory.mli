open Top.Specs

type sign

module type API = sig
  type assign
  val init: (sign,assign) slot_machine
  val clear: unit -> unit
end

include Theory.Type with type ('t,'v,'a) api = (module API with type assign = 'a)
                        
