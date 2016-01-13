open Kernel
open Top.Specs
open Theories_register
open PluginsTh_tools

open Dejan

type sign = MyTheory.sign
let hdl = Sig.Dejan

module ThDS = ThDS

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) 
  = MyTheory.Make(DS)
