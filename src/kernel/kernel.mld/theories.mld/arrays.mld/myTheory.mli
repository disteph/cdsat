open Theory

type sign

module type API = sig
  val init: sign slot_machine
  val clear: unit -> unit
end

val hdl : (sign*(module API)) Tags.t
