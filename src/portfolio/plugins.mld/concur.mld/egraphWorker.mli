open Async

open Kernel
open Interfaces

module Make(WB: WhiteBoardExt)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign = Theories.Eq.MyTheory.sign
           and type termdata = WB.DS.Term.datatype
           and type value  = WB.DS.Value.t
           and type cval   = WB.DS.CValue.t
           and type assign = WB.DS.Assign.t) : sig
  open WB
  val make : egraph ports -> unit Deferred.t
end
