(****************************************************)
(* This module is to be opened before anything else *)
(* as it redefines OCaml's standard stuff           *)
(****************************************************)

include Ppx_hash_lib.Std.Hash.Builtin
module Hash = struct
  let hash2fold h hash_state a = hash_fold_int hash_state (h a)
  let fold2hash f = Ppx_hash_lib.Std.Hash.run f
  let fold a = hash2fold Hashtbl.hash a
  let wrap1 hash_fold f = fold2hash (hash_fold (hash2fold f))
  let wrap2 hash_fold f g = fold2hash (hash_fold (hash2fold f) (hash2fold g))
end

module Pervasives = Pervasives

include Pervasives

module Boolhashed = struct
  type t = bool [@@deriving eq,ord,hash,show]
end

module Stringhashed = struct
  type t = string [@@deriving eq,ord,hash,show]
end

module Floathashed = struct
  type t = float [@@deriving eq,ord,hash,show]
end

let id2compare id a b = compare (id a) (id b)
let id2equal   id a b = (id a)=(id b)

let lex_compare compare1 compare2 (a1,a2) (b1,b2)
  = let c = compare1 a1 b1 in
    if c = 0 then compare2 a2 b2 else c

let min compare a1 a2 =
  if compare a1 a2 <= 0 then a1 else a2

let max compare a1 a2 =
  if compare a1 a2 >= 0 then a1 else a2
                                       
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

  let hash h = Hash.wrap1 hash_fold_t h
end
