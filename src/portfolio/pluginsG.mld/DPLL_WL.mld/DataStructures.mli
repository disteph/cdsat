open General
open Sums
open Patricia_interfaces

open Kernel
open Termstructures.Literals
open Theories.Prop
open Formulae

open Tools.PluginsG.SetInterface

module UASet : sig
  include CollectImplemExt with type e=LitF.t
  val cardinal : t->int
  val negations: t->t
  val latest   : t -> e option
  val choose   : t -> e
  val clear    : unit->unit
end

module UF : sig
  include FormulaF.Extra with type t = UASet.t
  val fset : t FormulaF.generic -> bool
end

module UFSet : sig
  include CollectImplemExt with type e = UF.t FormulaF.generic
  module UT: sig
    include Intern with type keys = e
    val compare : keys -> keys -> int
  end
  val iter    : (e -> unit) -> t -> unit
  val choose  : t -> e
  val rchoose : UASet.t -> t -> (e,e option)sum
  val clear   : unit->unit
end
