open General
open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Sums
open Tools.PluginsG.SetInterface


module UASet : sig
  include CollectImplemExt with type e=LitF.t
  val diff     : t -> t -> t
  val cardinal : t->int
  val negations: t->t
  val choose   : t -> e
  val clear    : unit->unit
end

module UF : FormulaF.Extra

module UFSet : sig
  include CollectImplemExt with type e = UF.t FormulaF.generic
  val choose  : t -> e
  val schoose : UASet.t -> t -> (e,e option)sum
  val rchoose : UASet.t -> t -> (e,e option)sum
  val clear   : unit->unit
end
