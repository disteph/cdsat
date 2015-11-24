open Kernel
open Top
open Specs
open Messages
open Theories_register

open Prop
open Literals

open IfThenElse

type sign = MyTheory.sign
let hdl = Sig.IfThenElse

module ThDS = struct
  type t = LitF.t 
  let bV tag _ = LitF.build(true,tag)
  let bC tag symb l = match symb,l with
    | Symbols.Neg,[a] -> LitF.negation a
    | _,_ ->  bV tag l
end

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) 
  = MyTheory.Make(DS)
