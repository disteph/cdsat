open General
open SetConstructions

module type Config = sig
  module Constraint: FromHConsed
  module Var: Map.OrderedType
  type fixed
  val pick_another: Constraint.t -> fixed -> Var.t -> Constraint.t*(Var.t option)
end

module Make(C : Config) : sig
  open C
  type t
  val init : t
  val addconstraint : Constraint.t -> Var.t -> Var.t -> t -> t
  val fix  : Var.t -> t -> t
  val next : fixed -> t -> Constraint.t option * t
end
