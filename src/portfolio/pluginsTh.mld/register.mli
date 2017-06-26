(* This is the register of all generic plugins in Psyche *)

open Kernel
open Top.Specs
open Theories.Register
    
module Make(DS: GlobalDS) : sig
  open DS
         
  val make : (Term.datatype*Value.t*Assign.t) Modules.t
             -> (Term.datatype*Value.t*Assign.t) PluginTh.sslot_machine * (unit -> unit)
end
