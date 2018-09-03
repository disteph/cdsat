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

module Value : PH

module Key : sig
  include Keys.S
  val make : (module Value with type t = 'a)
    -> 'a t * ('a -> Value.t) * (Value.t -> 'a option)
end

module CValue : sig
  type t[@@deriving show]
  include Hashtbl_hetero.T with type t := t
  val pp : t Format.printer
  val proj : 'a Key.t -> t -> 'a option
end

