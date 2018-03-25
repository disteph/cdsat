(****************************************************)
(* This module is to be opened before anything else *)
(* as it redefines OCaml's standard stuff           *)
(****************************************************)

module type Pervasives = sig
  val (=) : int -> int -> bool
  val (<>) : int -> int -> bool
  val (<) : int -> int -> bool
  val (>) : int -> int -> bool
  val (<=) : int -> int -> bool
  val (>=) : int -> int -> bool
  val compare : int -> int -> int
  val max: int -> int -> int
  val min: int -> int -> int
end

module Pervasives : sig
  include module type of Pervasives with type 'a ref = 'a ref
  include Pervasives
end

include Pervasives

module Compare : sig
  val max: ('a->'a->int) -> 'a -> 'a -> 'a
  val min: ('a->'a->int) -> 'a -> 'a -> 'a
  val lex: ('a->'a->int) -> ('b->'b->int) -> ('a*'b) -> ('a*'b) ->int
  val id2compare : ('a -> int) -> 'a -> 'a -> int
  val id2equal   : ('a -> int) -> 'a -> 'a -> bool
end

include module type of Ppx_hash_lib.Std.Hash.Builtin
                              
module Hash : sig
  val hash2fold : ('a -> int) -> 'a Ppx_hash_lib.Std.Hash.folder
  val fold2hash : 'a Ppx_hash_lib.Std.Hash.folder -> 'a -> int
  val fold      : 'a Ppx_hash_lib.Std.Hash.folder
  val wrap1 :
    ('a Ppx_hash_lib.Std.Hash.folder -> 'b Ppx_hash_lib.Std.Hash.folder) ->
    ('a -> int) -> 'b -> int
  val wrap2 :
    ('a Ppx_hash_lib.Std.Hash.folder -> 'b Ppx_hash_lib.Std.Hash.folder -> 'c Ppx_hash_lib.Std.Hash.folder) ->
    ('a -> int) -> ('b -> int) -> 'c -> int
end

module Boolhashed : sig
  type t = bool [@@deriving eq,ord,hash,show]
end

module Stringhashed : sig
  type t = string [@@deriving eq,ord,hash,show]
end

module Floathashed : sig
  type t = float [@@deriving eq,ord,hash,show]
end

module List : sig
  include module type of List
  type 'a t = 'a list [@@deriving eq, hash, show]
  val pp : ?sep:string -> ?wrap:string*string
           -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val hash : ('a -> int) -> 'a list -> int
  val mem  : ('a -> 'a -> bool) -> 'a -> 'a list -> bool
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val last : 'a t -> 'a
end

