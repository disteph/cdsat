open Kernel
open Top
open Specs
open Messages
open Theories_register
open Num

open Dejan

type sign = MyTheory.sign
let hdl = Sig.Dejan

module ThDS = MyTheory.ThDS

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) 
  = MyTheory.Make(DS)
