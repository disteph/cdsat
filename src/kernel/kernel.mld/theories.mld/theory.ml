open Top
open Specs

module type DSproj = sig
  include GTheoryDSType
  type ts
  val proj: Term.datatype -> ts
end

module type Type = sig

  type ('term,'tset) t

  type ts
  val ts : ts Termstructures.Register.TSHandler.t

  module Make(DS: DSproj with type ts := ts) : sig
    open DS
    val theorymodule : (Term.t,TSet.t) t
  end

end
