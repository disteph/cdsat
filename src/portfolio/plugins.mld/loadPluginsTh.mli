open Kernel
open PluginsTh.PluginTh

module Make(K:Export.API) : sig

  val pluginsTh : K.WB.DS.Assign.t sslot_machine Theories.Register.HandlersMap.t
  val clear : unit -> unit
    
end
