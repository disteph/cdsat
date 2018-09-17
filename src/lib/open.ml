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

module Compare = struct
  type 'a t = 'a->'a->int
  let min compare a1 a2 =
    if compare a1 a2 <= 0 then a1 else a2
  let max compare a1 a2 =
    if compare a1 a2 >= 0 then a1 else a2
  let lex compare1 compare2 (a1,a2) (b1,b2)
    = let c = compare1 a1 b1 in
    if c = 0 then compare2 a2 b2 else c
  let id2compare id a b = compare (id a) (id b)
end

module Equal = struct
  type 'a t = 'a->'a->bool
  let id2equal   id a b = (id a)=(id b)
end

module Pervasives = Pervasives

include Pervasives

let (>>) f g x = x |> f |> g
let int_pairing x y = let z = y+((x+1)/2) in x+(z*z)

include Ppx_hash_lib.Std.Hash.Builtin

module Hash = struct
  open Ppx_hash_lib.Std.Hash
  type 'a t      = 'a -> int
  type nonrec state = state
  type nonrec 'a folder = 'a folder
  let hash2fold h hash_state a = hash_fold_int hash_state (h a)
  let fold2hash = of_fold
  let fold a = hash2fold Hashtbl.hash a
  let pair (type a) (type b) hash_fold_a hash_fold_b = [%hash_fold:a*b]
  let triple (type a) (type b) (type c) hash_fold_a hash_fold_b hash_fold_c
    = [%hash_fold:a*b*c]
  let wrap1 hash_fold f = fold2hash (hash_fold (hash2fold f))
  let wrap2 hash_fold f g = fold2hash (hash_fold (hash2fold f) (hash2fold g))
end

module Format = struct
  include Format
  type 'a printer = formatter -> 'a -> unit
  type 'a spec = ('a, Format.formatter, unit) format -> 'a

  let toString a =
    let buf = Buffer.create 255 in
    let fmt = Format.formatter_of_buffer buf in
    a (Format.fprintf fmt);
    Format.fprintf fmt "%!";
    Buffer.contents buf

  let stringOf f a = toString (fun p->p "%a" f a)

end

module Boolhashed = struct
  type t = bool [@@deriving eq,ord,hash,show]
end

module Stringhashed = struct
  type t = string [@@deriving eq,ord,hash]
  let pp fmt s = Format.fprintf fmt "%s" s
  let show = Format.stringOf pp
end

module Floathashed = struct
  type t = float [@@deriving eq,ord,hash,show]
end

module List = struct

  include List
            
  type 'a t = 'a list [@@deriving eq, hash, show]

  let pp ?(sep="; ") ?(wrap="[","]") f fmt l =
    let rec aux fmt = function
    | []   -> Format.fprintf fmt ""
    | [a]  -> Format.fprintf fmt "%a" f a
    | a::l -> Format.fprintf fmt "%a%s%a" f a sep aux l
    in
    let b,e = wrap in
    Format.fprintf fmt "%s%a%s" b aux l e
       
  let rec mem eq x = function
    | [] -> false
    | y::l when eq x y -> true
    | _::l -> mem eq x l

  let fold f seed l = List.fold_left (fun sofar elt -> f elt sofar) l seed

  let rec last = function
    | [] -> failwith "last"
    | [h] -> h
    | h :: t -> last t

end
