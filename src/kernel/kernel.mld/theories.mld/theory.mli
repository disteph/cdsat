(**************************************************************)
(* This is the specification of of theory module, kernel part *)
(**************************************************************)

open General
open Top

include module type of Theory_sig

module Tags : sig
  type 'a t constraint 'a = _*_
  val compare : _ t -> _ t -> int
  val pp : _ t Format.printer
  val id : _ t -> int
  val hash   : _ t Hash.t
  val equal  : ('a*_) t -> ('b*_) t -> ('a,'b) General.Poly.iseq
  val eq     : ('a*_) t -> ('b*_) t -> ('a,'b) General.Poly.eq
  val coerce : ('a*_) t -> ('b*_) t -> 'a -> 'b
  val make   : (_*'a) t -> (module Terms.Writable) -> 'a
end

module Handlers: sig
  type t =
    | Handler : _ Tags.t -> t
    | Eq
  [@@deriving ord, show]
  val id : t -> int
end

val register :
  (module WithSign with type api = 'api and type sign = 'sign) -> ('sign*'api) Tags.t

val all_theories_list : Handlers.t list ref

val fail_state : _ slot_machine
