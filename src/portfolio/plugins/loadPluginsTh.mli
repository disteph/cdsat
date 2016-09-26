open Kernel
open Top.Specs
open Theories_register
open Combo

type _ sslot_machine = Signed: 'sign Sig.t * ('sign,'ts) slot_machine -> 'ts sslot_machine

module type GetPlugins = sig
  include Plugin.DataList
  val make :
    (module GTheoryDSType with type Term.datatype = 't and type TSet.t = 'ts)
    -> ('t,agglo) projList
    -> 'ts sslot_machine HandlersMap.t * (unit->unit)
end

val make : unit HandlersMap.t -> (module GetPlugins)
