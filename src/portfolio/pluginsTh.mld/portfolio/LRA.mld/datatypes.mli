open Format
       
open General
open Patricia

open Kernel
open Top.Terms
open Top.Messages
open Top.Sassigns
    
open Theories.LRA
       
open Tools

module Make(W: Writable)(K: API.API with type sign = MyTheory.sign) : sig

  module Domain : Map.S
         with type keys   = Term.t
          and type values = BAssign.t Range.t
          and type infos  = Term.t option
          and type common = int
          and type branching = int

  module ConfigB : TwoWatchedLits.Config
         with type Constraint.t = K.Simpl.t * bool
          and type Var.t = Term.t
          and type 'a M.t = K.Model.t -> 'a

  module WLB : sig
    type t
    val init : t
    val flush : t -> t
    val fix : ConfigB.Var.t -> t -> t
    val addconstraint : ConfigB.Constraint.t -> watched:ConfigB.Var.t list -> t -> t
    val addconstraintNflag :
      ConfigB.Constraint.t -> ?ifpossible:ConfigB.Var.t list -> t -> t
    val next :
      ?howmany:int ->
      t ->
      ((ConfigB.Var.t list, ConfigB.Constraint.t * ConfigB.Var.t list)
        General.Sums.sum * t) ConfigB.M.t
  end

  module ConfigQ : TwoWatchedLits.Config
         with type Constraint.t = K.Simpl.t * Kernel.Top.Qhashed.t option * Term.t
          and type Var.t = Term.t
          and type 'a M.t = K.Model.t -> 'a

  module WLQ : sig
    type t
    val init : t
    val flush : t -> t
    val fix : ConfigQ.Var.t -> t -> t
    val addconstraint : ConfigQ.Constraint.t -> watched:ConfigQ.Var.t list -> t -> t
    val addconstraintNflag :
      ConfigQ.Constraint.t -> ?ifpossible:ConfigQ.Var.t list -> t -> t
    val next :
      ?howmany:int ->
      t ->
      ((ConfigQ.Var.t list, ConfigQ.Constraint.t * ConfigQ.Var.t list)
        General.Sums.sum * t) ConfigQ.M.t
  end

  val pp_beval : (K.sign,straight) message Format.printer
  val pp_fm    : (BAssign.t*BAssign.t*Term.t) Format.printer
  val pp_diseq :
    (BAssign.t*BAssign.t*BAssign.t*(K.sign,straight) message*(K.sign,straight) message) Format.printer

end
