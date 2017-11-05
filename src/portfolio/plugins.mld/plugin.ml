open PluginsTh.PluginTh
       
module type Input = sig
  include  Kernel.Export.API
  open WB.DS
  val pluginsTh : (Term.datatype*Value.t*Assign.t*TSet.t) sslot_machine list
  val clear : unit -> unit
end
       
module type API = sig
  module Make(K : Input) : sig
    open K
    val solve : WB.DS.Assign.t
                -> (Kernel.Top.Messages.unsat WB.t, WB.sat_ans) General.Sums.sum
    val clear : unit -> unit
  end
end
