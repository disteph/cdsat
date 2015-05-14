open General.Sums
open Kernel.Interfaces_basic
open Kernel.Interfaces_theory
open Kernel.Formulae
open Kernel.Interfaces_plugin
open SetInterface

module Make
  (IAtom:AtomType)
  (FE: FrontEndType with type ASet.e = IAtom.t)
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
