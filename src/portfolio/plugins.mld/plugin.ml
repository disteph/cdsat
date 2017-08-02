open PluginsTh.PluginTh
       
module type Input = sig
  include  Kernel.Export.APIext
  open WB.DS
  val pluginsTh : (Term.datatype*Value.t*Assign.t*TSet.t) sslot_machine list
  val clear : unit -> unit
end
       
module type API = sig
  module Make(K : Input) : sig
    val solve : unit -> K.answer
    val clear : unit -> unit
  end
end
