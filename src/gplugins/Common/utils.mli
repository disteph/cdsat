open Lib.Sums
open Kernel.Interfaces_I
open Kernel.Formulae
open Kernel.Interfaces_II
open SetInterface

module PHCons_ext(A:PHCons) : Hashtbl.HashedType with type t = A.t

module FEext(FE:FrontEndType): sig
  open FE
  val accept  :receive
  val fNone   :alt_action
  val isProvable   :t->bool
  val isNotProvable:t->bool
  val model   :Seq.t->asetType
end


module Memo
  (IAtom:IAtomType)
  (FE:FrontEndType with type dsubsts = IAtom.DSubst.t and type Form.lit = IAtom.Atom.t)
  (FSet: CollectImplemExt with type e = (FE.Form.datatype,FE.Form.lit) GForm.t * IAtom.DSubst.t and type t=FE.fsetType)
  (ASet: CollectImplemExt with type e = IAtom.t and type t=FE.asetType)
  : sig
    open FE
    val tomem          : t -> unit

    val get_usage_stats4provable : t->int 
    val reset_stats4provable : t->unit
    val search4provableNact : Seq.t->alt_action->alt_action
    val search4notprovableNact : Seq.t->(unit->focusaction)->focusaction

    val report         : unit->unit
    val clear          : unit->unit
    val size           : unit->int
  end
