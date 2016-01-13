open General
open SetConstructions
open Kernel.Top.Messages

module type Config = sig

  include TwoWatchedLits.Config

  type sign
  type tset
  type constraints

  val init_fixed : fixed
  val init_constraints : constraints

  type result = 
    | UNSAT of (sign,tset,thProvable) thsays
    | Propagate of fixed * Var.t
    | Meh

  val extract_msg: constraints -> (sign,tset,thStraight) thsays option 
  val constreat : Constraint.t -> constraints 
    -> (constraints * result, Var.t * Var.t) Sums.sum

end

module Make(C: Config) : sig
  type t 
  val init  : t
  val treat :
    C.Constraint.t -> t ->
    (C.sign, C.tset, thStraight) thsays option *
      ((C.sign, C.tset, thProvable) thsays, t) Sums.sum
end
