open General
open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Sums
open Patricia_interfaces
open Tools.PluginsG.SetInterface

module UASet : sig
  include CollectImplemExt with type e=LitF.t
                           and type t=(LitF.t,unit,int,int,LitF.t Patricia.m_infos) Patricia.poly
  val print_in_fmt : Format.formatter -> t -> unit
  val cardinal : t->int
  val negations: t->t
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
  val print_in_fmt : Format.formatter -> t -> unit
  val iter    : (e -> unit) -> t -> unit
  val choose  : t -> e
  val rchoose : UASet.t -> t -> (e,e option)sum
  val clear   : unit->unit
end
