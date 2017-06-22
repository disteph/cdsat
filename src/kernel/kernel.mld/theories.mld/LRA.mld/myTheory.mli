open Top.Specs

type sign

module type API = sig
  type assign
  type sassign
  val init: (sign,assign,sassign) slot_machine
  val clear: unit -> unit
end

include Theory.Type
        with type ('t,'v,'a) api = (module API with type assign = 'a
                                                and type sassign= ('t,'v)sassign)
                        
