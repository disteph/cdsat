open Kernel
open Export
open Top.Specs
open Theories.Register

type _ sslot_machine =
  Signed:
    ('tva*('sign*_*_*_)) Tags.t
  * ('sign,'tva) slot_machine
  -> 'tva sslot_machine

type ('sign,'tva) pluginTh = {
    init:('sign,'tva) slot_machine;
    clear: unit -> unit
  }
       
module type Type = sig
  type sign
  type (_,_,_) api
  module Make(WB: WhiteBoard) : sig
    open WB
    open DS
    val make: (Term.datatype,Value.t,Assign.t) api
              -> (sign,Term.datatype*Value.t*Assign.t) pluginTh
  end
end
