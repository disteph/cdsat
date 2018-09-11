open Theory

type sign

module type API = sig
  val init: sign slot_machine
end

val hdl : (sign*(module API)) Tags.t
