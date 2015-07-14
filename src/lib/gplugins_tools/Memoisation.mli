open General.Sums
open Kernel.Top.Interfaces_basic
open Kernel.Prop.Interfaces_theory
open Kernel.Prop.Formulae
open Kernel.Prop.Interfaces_plugin
open SetInterface

module Make
  (FE: FrontEndType)
  (FS: CollectImplemExt with type e = FE.FSet.e and type t=FE.FSet.ps)
  (AS: CollectImplemExt with type e = FE.ASet.e and type t=FE.ASet.ps)
  : sig
    open FE
    val tomem          : answer -> unit

    val get_usage_stats4provable : answer->int 
    val reset_stats4provable     : answer->unit
    val search4provableNact    : Seq.t-> ('a address*'a address) -> 'a alt_action->'a alt_action
    val search4notprovableNact : Seq.t->(unit->'a focusCoin)->'a focusCoin

    val report         : unit->unit
    val clear          : unit->unit
    val size           : unit->int
  end
