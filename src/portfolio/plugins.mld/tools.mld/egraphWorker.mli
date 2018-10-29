open Async

open Kernel

module Make(WB: WhiteBoardExt.S)
    (EGraph: Theories.Eq.MyTheory.API
     with type sign := Theories.Eq.MyTheory.sign) : sig
  open WhiteBoardExt
  open WB
  val make : egraph ports -> unit Deferred.t
end
