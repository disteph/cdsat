open Kernel.Interfaces_basic
open Kernel.Interfaces_plugin

module PHCons_ext(A:PHCons) : Hashtbl.HashedType with type t = A.t

module FEext(FE:FrontEndType): sig
  open FE
  val accept  : receive
  val fNone   : 'a alt_action
  val isProvable   :answer->bool
  val isNotProvable:answer->bool
  val model   :Seq.t->ASet.ps
end
