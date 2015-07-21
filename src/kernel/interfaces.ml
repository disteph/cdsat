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
  type hubsays = HubSays of TSet.t*(TSet.t->TSet.t)
  module type InsertCoin = sig
    type t
    val thdone : 'a Register.t -> ('a,TSet.t) thdone -> t
    val thsays : 'a Register.t -> ('a,TSet.t) thsays -> hubsays*t
  end
  type answer = Provable of TSet.t | NotProvable of TSet.t
  type output = Jackpot of answer | InsertCoin of (module InsertCoin with type t=output)
  val consistency     :           TSet.t -> output
  val goal_consistency: Term.t -> TSet.t -> output
end
