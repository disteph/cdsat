open Top
open Specs
open Messages

open Prop
open Literals

type sign = CCX.sign

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> LitF.t
end) 
  = CCX.Make
  (DS)
  (EmptyCC.Make(DS))
  (MyPUF.Make(struct include DS.Term let compare = Terms.compare end))
