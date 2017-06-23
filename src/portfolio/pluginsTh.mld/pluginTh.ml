open Kernel
open Top.Specs
open Theories.Register

type _ sslot_machine =
  Signed:
    (_*('sign*_*_*_)) Tags.t
  * ('sign,'assign,'sassign) slot_machine
  -> ('assign*'sassign) sslot_machine

type ('sign,'a,'sa) pluginTh = {
    init:('sign,'a,'sa) slot_machine;
    clear: unit -> unit
  }
       
module type Type = sig
  type sign
  type (_,_,_) api
  module Make(DS: GlobalDS) : sig
    open DS
    val make: (Term.datatype,Value.t,Assign.t) api
              -> (sign,DS.Assign.t,(Term.datatype,Value.t) sassign) pluginTh
  end
end
