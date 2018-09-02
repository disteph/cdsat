module type NamedType = sig
  type t
  val name : string
end

exception BadCoercion
(** Raised when making a bad coercion *)

module type S = sig

  type _ t [@@deriving ord,show]

  val id  : 'a t -> int
  val hash: 'a t -> int

  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b} [@@unboxed]
  val fold : 'b fold -> 'b -> 'b

  val equal  : 'a t -> 'b t -> ('a,'b) Poly.iseq
  val eq     : 'a t -> 'b t -> ('a,'b) Poly.eq
  val coerce : 'a t -> 'b t -> 'a -> 'b

  val create_key: (module NamedType with type t = 'a) -> 'a t

end
