open Format
       
open General
open Patricia
open Patricia_interfaces

open Kernel
open Export
open Theories.LRA
open Top.Messages
       
open Tools.PluginsTh
       
module Make(DS: GlobalImplem)
         (K: API.API with type sign   = MyTheory.sign
                      and type assign = DS.Assign.t
                      and type termdata= DS.Term.datatype
                      and type value  = DS.Value.t
                      and type tset   = DS.TSet.t ) : sig

  open DS

  type datatypes = Term.datatype * Value.t * Assign.t * TSet.t

  module Domain : PatMap
         with type keys   = int
          and type values = bassign Range.t
          and type infos  = int option
          and type common = int
          and type branching = int
          and type ('v,'i) param = (int,'v,int,int,'i) poly

  module ConfigB : TwoWatchedLits.Config
         with type Constraint.t = K.Simpl.t * bool
          and type Var.t = int
          and type fixed = K.Model.t

  module WLB : sig
    type t
    val init : t
    val flush : t -> t
    val fix : ConfigB.Var.t -> t -> t
    val addconstraint : ConfigB.Constraint.t -> watched:ConfigB.Var.t list -> t -> t
    val addconstraintNflag :
      ConfigB.Constraint.t -> ?ifpossible:ConfigB.Var.t list -> t -> t
    val next :
      ConfigB.fixed ->
      ?howmany:int ->
      t ->
      (ConfigB.Var.t list, ConfigB.Constraint.t * ConfigB.Var.t list)
        General.Sums.sum * t
  end

  module ConfigQ : TwoWatchedLits.Config
         with type Constraint.t = K.Simpl.t * Kernel.Top.Qhashed.t option * int
          and type Var.t = int
          and type fixed = K.Model.t

  module WLQ : sig
    type t
    val init : t
    val flush : t -> t
    val fix : ConfigQ.Var.t -> t -> t
    val addconstraint : ConfigQ.Constraint.t -> watched:ConfigQ.Var.t list -> t -> t
    val addconstraintNflag :
      ConfigQ.Constraint.t -> ?ifpossible:ConfigQ.Var.t list -> t -> t
    val next :
      ConfigQ.fixed ->
      ?howmany:int ->
      t ->
      (ConfigQ.Var.t list, ConfigQ.Constraint.t * ConfigQ.Var.t list)
        General.Sums.sum * t
  end

  val pp_beval : Format.formatter -> (K.sign,straight) Msg.t -> unit
  val pp_fm    : Format.formatter -> (bassign*bassign*Term.t) -> unit
  val pp_diseq :
    Format.formatter
    -> (bassign*bassign*bassign*(K.sign,straight) Msg.t*(K.sign,straight) Msg.t)
    -> unit

end
