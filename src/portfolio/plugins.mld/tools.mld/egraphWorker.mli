open Async

open Kernel
open Interfaces

module Make(WB: WhiteBoardExt)
    (EGraph: Theories.Eq.Interfaces.API
     with type sign := Theories.Eq.MyTheory.sign) : sig
  open WB
  val make : egraph ports -> unit Deferred.t
end
