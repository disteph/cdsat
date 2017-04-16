open Top
open Specs
open Messages

open Termstructures.Literals

type sign = CCX.sign

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> LitF.t
end) 
  = CCX.Make
  (DS)
  (EmptyCC.Make(DS))
  (MyPUF.Make(DS.Term))
