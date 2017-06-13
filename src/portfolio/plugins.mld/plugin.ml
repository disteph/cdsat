open Kernel
open PluginsTh.PluginTh
       
module type Type = sig

  (* A plugin should provide an implementation of formulae, an
     implementation of sets of formulae, and an implementation of
     sets of atoms *)
  
  module DS : Theories.Prop.APIplugin.PlugDSType

  module Make(K: Kernel.Export.API) : sig
    val solve : unit -> K.answer
    val clear : unit -> unit
  end
end

module type IPluginProp = sig

  module FE: Theories.Prop.APIplugin.FrontEnd
  open FE
  type data
  val initial_data : seqU seq -> bool list -> data
  val solve        : data output -> seqU answer
end

module type Input = sig
  include  Kernel.Export.API
  module IPluginProp : IPluginProp
  val pluginsTh : WB.DS.Assign.t sslot_machine Theories.Register.HandlersMap.t
  val clear : unit -> unit
end
       
module type API = sig
  module Make(K : Input) : sig
    val solve : unit -> K.answer
    val clear : unit -> unit
  end
end
