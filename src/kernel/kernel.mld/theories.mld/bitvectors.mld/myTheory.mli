open Top.Specs
open Top.Messages
       
type sign

module V : sig
  include HardCaml.Comb.S
  val isT : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Ppx_hash_lib.Std.Hash.folder
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
       
module type API = sig
  type termdata
  type assign
  type state
  val init: state
  val term_eval : (int -> V.t) -> termdata termF -> V.t
  val form_eval : (int -> V.t) -> termdata termF -> bool
  val eval : state -> termdata termF
             -> (sign, assign * (termdata termF * bool), straight) message
end


include Theory.Type
        with type ('t,'v,'a) api = (module API with type termdata = 't
                                                and type assign = 'a)
