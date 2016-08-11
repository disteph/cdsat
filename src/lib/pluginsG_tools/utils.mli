open Kernel.Top.Interfaces_basic
open Kernel.Prop.Interfaces_plugin

module PHCons_ext(A: sig type t val id : t -> int end)
       : Hashtbl.HashedType with type t = A.t

module FEext(FE:FrontEndType): sig
  open FE
  val accept  : receive
  val fNone   : 'a alt_action
  val isProvable   :'a answer->bool
  val isNotProvable:'a answer->bool
  val model   : seqU seq->ASet.ps
end
