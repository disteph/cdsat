open Kernel
open PluginsTh.PluginTh
       
module type Type = sig
  module Make(K: Kernel.Export.APIext) : sig
    val solve : unit -> K.answer
    val clear : unit -> unit
  end
end


module type Input = sig
  include  Kernel.Export.APIext
  open WB.DS
  val pluginsTh : (Term.datatype*Value.t*Assign.t) sslot_machine Theories.Register.HandlersMap.t
  val clear : unit -> unit
end
       
module type API = sig
  module Make(K : Input) : sig
    val solve : unit -> K.answer
    val clear : unit -> unit
  end
end
