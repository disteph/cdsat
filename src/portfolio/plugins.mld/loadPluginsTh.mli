open Kernel

module Make(K:Export.API) : sig

  val pluginsTh : K.WB.DS.Assign.t Plugin.sslot_machine Theories.Register.HandlersMap.t
  val clear : unit -> unit
    
end
