open Top.Interfaces_basic
open Top.Messages

module type GTheoryDSType = sig
  module Term : Top.Specs.Term
  type formulaF
  val asF : Term.datatype -> formulaF
  module TSet : CollectTrusted with type e = Term.t
end

module type WhiteBoard = sig
  module DS : GTheoryDSType
  open DS
  module type InsertCoin = sig
    type t
    val take : 'a Register.t -> ('a,TSet.t) message -> t
    (* val gimmeFreshEigen: Sorts.t -> World.FreeVar.t * t *)
  end
  type answer = private Provable of TSet.t | NotProvable of TSet.t
  type output = Jackpot of answer | InsertCoin of (module InsertCoin with type t=output)
  val consistency     :           TSet.t -> answer
  val goal_consistency: Term.t -> TSet.t -> answer
end
