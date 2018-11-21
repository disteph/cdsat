open General

open Kernel
open Top.Terms

open Theories.Theory
open Theories.Eq
open MyTheory

module type WatchStruct = sig
  include Top.Basic.PH
  val name : string
end

module Key : sig
  type _ t [@@deriving ord, show]
  val id : 'a t -> int
  val hash : 'a t -> int
  type 'b fold = { fold : 'a. 'a t -> 'b -> 'b; } [@@unboxed]
  val fold : 'b fold -> 'b -> 'b
  val equal  : 'a t -> 'b t -> ('a, 'b) Poly.iseq
  val eq     : 'a t -> 'b t -> ('a, 'b) Poly.eq
  val coerce : 'a t -> 'b t -> 'a -> 'b
  val make : (module WatchStruct with type t = 'a) -> 'a t
end

type t [@@deriving show]
val id        : t -> int
val watchstruct : 'a Key.t -> t -> 'a
val howmany   : t -> int
val among     : t -> TSet.t
val watched   : t -> Term.t list
val valuation : t -> sign Valuation.signed
val make      : 'a Key.t -> 'a -> _ Top.Values.Key.t -> howmany:int -> among:TSet.t -> t
val simplify  : t -> (sign self*Term.t list) -> t*(sign self*Term.t list)
