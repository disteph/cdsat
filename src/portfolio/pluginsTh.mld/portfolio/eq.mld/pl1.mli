open Kernel
open Tools
open Top
open Terms
open Values
open Sassigns
open Messages
open Theories.Eq.MyTheory

type state

val add : SAssign.t -> level:int -> state -> state

val share : TSet.t -> state -> state

val ask : ?subscribe:bool -> node -> state ->
  (Term.t * CValue.t * (unit -> CValue.t list)) option * state

val watchthis : Constraint.t -> state -> state

type speak =
  | Nothing
  | Quid    : Term.t -> speak
  | Propa   : (sign,_ propa) imessage -> speak
  | Sat     : (sign,sat) message -> speak
  | Detect  : Constraint.t -> speak
    
val speak : state -> speak * state
  
module Make(Kern: API with type sign := sign) : sig
  val init : state      
end
