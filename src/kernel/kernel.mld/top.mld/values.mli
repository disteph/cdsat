(**********)
(* Values *)
(**********)

open General
open Basic
    
type nbool = private NBool
       
type ('v,_) t =
  | NonBoolean : 'v -> ('v,nbool) t
  | Boolean    : bool -> ('v,bool) t
[@@deriving eq,ord,show]

val hash_fold_t : ('v Hash.folder) -> (('v, _) t) Hash.folder

type 'v values = Values : ('v,_) t -> 'v values [@@unboxed]
[@@deriving eq,ord,hash,show]

module type Value = sig
  include Basic.PH
  val name : string
end

module Key : sig
  include Keys.S
  val make : (module Value with type t = 'a) -> 'a t
end

module Value : sig
  include PH
  val inj  : 'a Key.t -> 'a -> t
  val proj : 'a Key.t -> t -> 'a option
end

module CValue : sig
  type t[@@deriving eq,show]
  val none : Sorts.t -> t
  val proj : 'a Key.t -> t -> 'a values option
  val inj  : Value.t values -> t
  val merge : t -> t -> (Value.t values*Value.t values,t) Sums.sum
end
