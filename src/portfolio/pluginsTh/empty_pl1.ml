open Kernel
open Top
open Specs
open Messages
open Theories_register

open Empty

type sign = Empty.MyTheory.sign
let hdl = Sig.Empty

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
