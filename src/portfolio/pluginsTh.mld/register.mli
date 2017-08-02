(* This is the register of all generic plugins in Psyche *)

open Kernel
open Export
open Top.Specs
open Theories.Register
    
module Make(DS: GlobalImplem) : sig
  open DS
         
  val make : (Term.datatype*Value.t*Assign.t*TSet.t) Modules.t
             -> (Term.datatype*Value.t*Assign.t*TSet.t) PluginTh.sslot_machine * (unit -> unit)
end
