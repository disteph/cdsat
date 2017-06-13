open Kernel
open Top.Specs
open Theories.Register

type _ sslot_machine =
  Signed:
    (_*('sign*_*_*_)) Tags.t
  * ('sign,'ts) slot_machine
  -> 'ts sslot_machine

type ('sign,'a) pluginTh = {
    init:('sign,'a) slot_machine;
    clear: unit -> unit
  }
       
module type Type = sig
  type sign
  type (_,_,_) api
  module Make(DS: GlobalDS) : sig
    open DS
    val make: (Term.datatype,Value.t,Assign.t) api -> (sign,DS.Assign.t) pluginTh
  end
end
