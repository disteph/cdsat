(****************************************************)
(* This module is to be opened before anything else *)
(* as it redefines OCaml's standard stuff           *)
(****************************************************)

val (=) : int -> int -> bool
val (<>) : int -> int -> bool
val (<) : int -> int -> bool
val (>) : int -> int -> bool
val (<=) : int -> int -> bool
val (>=) : int -> int -> bool
val compare : int -> int -> int
val id2compare : ('a -> int) -> 'a -> 'a -> int
val id2equal   : ('a -> int) -> 'a -> 'a -> bool
type _ compare =
  | Compare : ('a->'a->int)-> 'a compare
  | IntCompare : int compare
val max: ?using:'a compare -> 'a -> 'a -> 'a
val min: ?using:'a compare -> 'a -> 'a -> 'a

module Pervasives : sig
  val compare : int -> int -> int
  val max : int -> int -> int
  val exit : int -> 'a
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

val lex_compare : ('a->'a->int) -> ('b->'b->int) -> ('a*'b) -> ('a*'b) ->int
                      
module List : sig
  type 'a t = 'a list [@@deriving eq, hash, show]
  val hash : ('a -> int) -> 'a list -> int
  val mem : ('a -> 'a -> bool) -> 'a -> 'a list -> bool
  val map : ('a -> 'b) -> 'a list -> 'b list
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val for_all : ('a -> bool) -> 'a list -> bool
  val rev : 'a list -> 'a list
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val iteri: (int -> 'a -> unit) -> 'a list -> unit
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
end

val hash_fold_nativeint : nativeint Base__Hash.folder
val hash_fold_int64 : int64 Base__Hash.folder
val hash_fold_int32 : int32 Base__Hash.folder
val hash_fold_char : char Base__Hash.folder
val hash_fold_int : int Base__Hash.folder
val hash_fold_bool : bool Base__Hash.folder
val hash_fold_string : string Base__Hash.folder
val hash_fold_float : float Base__Hash.folder
val hash_fold_unit : unit Base__Hash.folder
val hash_fold_option : 'a Base__Hash.folder -> 'a option Base__Hash.folder
val hash_fold_list : 'a Base__Hash.folder -> 'a list Base__Hash.folder
val hash_fold_lazy_t : 'a Base__Hash.folder -> 'a lazy_t Base__Hash.folder
val hash_fold_ref_frozen :
  'a Base__Hash.folder -> 'a Base__.Import0.ref Base__Hash.folder
val hash_fold_array_frozen :
  'a Base__Hash.folder -> 'a array Base__Hash.folder
                              
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
