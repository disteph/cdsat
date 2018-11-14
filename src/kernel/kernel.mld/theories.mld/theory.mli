(**************************************************************)
(* This is the specification of of theory module, kernel part *)
(**************************************************************)

open General
open Top
open Terms
    
include module type of Theory_sig

module Tags : sig
  type 'a t
  val compare : _ t -> _ t -> int
  val pp : _ t Format.printer
  val id : _ t -> int
  val hash   : _ t Hash.t
  val equal  : 'a t -> 'b t -> ('a,'b) General.Poly.iseq
  val eq     : 'a t -> 'b t -> ('a,'b) General.Poly.eq
  val make   : (_*'a) t -> (module Terms.Writable) -> 'a
  val dsKeys : (_*_) t -> dsKey list
end

module Handlers: sig
  type t =
    | Handler : (_*_) Tags.t -> t
    | Eq
  [@@deriving ord, show]
  val id : t -> int
end

val register :
  (module Type with type api = 'api and type sign = 'sign) -> ('sign*'api) Tags.t

val all_theories_list : Handlers.t list ref
