open Kernel
open Top.Specs
open Theories_register

open Arrays

type sign = MyTheory.sign
let hdl = Sig.Arrays

module ThDS = struct
  type t = unit 
  let bC _ _ _ = ()
  let bV _ _   = ()
end

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) 
  = MyTheory.Make(DS)
