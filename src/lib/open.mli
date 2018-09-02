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
  type 'a t = 'a->'a->int
  val max: 'a t -> 'a -> 'a -> 'a
  val min: 'a t -> 'a -> 'a -> 'a
  val lex: 'a t -> 'b t -> ('a*'b) t
  val id2compare : ('a -> int) -> 'a -> 'a -> int
end

module Equal : sig
  type 'a t = 'a->'a->bool
  val id2equal   : ('a -> int) -> 'a -> 'a -> bool
end

val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

include module type of Ppx_hash_lib.Std.Hash.Builtin
                              
module Hash : sig
  open Ppx_hash_lib.Std
  type 'a t      = 'a -> int
  type state     = Hash.state
  type 'a folder = 'a Hash.folder
  val hash2fold : ('a -> int) -> 'a folder
  val fold2hash : 'a folder -> 'a t
  val fold      : 'a folder
  val pair      :  'a folder -> 'b folder -> ('a*'b) folder
  val triple    :  'a folder -> 'b folder -> 'c folder -> ('a*'b*'c) folder
  val wrap1     : ('a folder -> 'b folder) -> 'a t -> 'b t
  val wrap2     :
    ('a Hash.folder -> 'b Hash.folder -> 'c Hash.folder) -> 'a t -> 'b t -> 'c t
end

module Format : sig
  include module type of Format
  with type formatter = Format.formatter
   and type symbolic_output_buffer = Format.symbolic_output_buffer
  type 'a printer = formatter -> 'a -> unit
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
           -> ('a Format.printer) -> 'a t Format.printer
  val mem  : ('a -> 'a -> bool) -> 'a -> 'a list -> bool
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val last : 'a t -> 'a
end
