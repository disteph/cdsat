open Kernel.Top.Messages
open Kernel.Top.Sassigns
open PluginsTh.PluginTh
       
module type Input = sig
  include  Kernel.Combo.API
  val pluginsTh : sslot_machine list
  val clear : unit -> unit
end
       
module type API = sig
  module Make(K : Input) : sig
    open K
    val solve : Assign.t -> (unsat WB.t, WB.sat_ans) General.Sums.sum
    val clear : unit -> unit
  end
end
