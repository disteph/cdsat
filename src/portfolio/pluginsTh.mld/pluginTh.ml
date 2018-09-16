open Kernel
open Combo
open Theories.Register
open Theories.Theory

type sslot_machine = Signed: ('sign*_) Tags.t * 'sign slot_machine -> sslot_machine

type 'sign pluginTh = {
    init : 'sign slot_machine;
    clear: unit -> unit
  }
       
module type Type = sig
  type sign
  type api
  val hdl : (sign*api) Tags.t
  module Make(W: Top.Terms.Writable) : sig
    val make : api -> sign pluginTh
  end
end
