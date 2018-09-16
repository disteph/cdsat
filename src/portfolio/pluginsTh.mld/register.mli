open Kernel
open Top.Terms
open Theories.Theory
open Theories.Register
    
exception NoPluginTh of Handlers.t

module Make(W: Writable) : sig
  val make : Modules.t -> PluginTh.sslot_machine * (unit -> unit)
end
