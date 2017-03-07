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

let id2compare id a b = compare (id a) (id b)
let id2equal   id a b = (id a)=(id b)
      
type _ compare =
  | Compare : ('a->'a->int)-> 'a compare
  | IntCompare : int compare

let min (type a) ?(using: a compare option) a1 a2 =
  match using with
  | Some(Compare compare) -> if compare a1 a2 <= 0 then a1 else a2
  | _ -> if a1 <= a2 then a1 else a2

let max (type a) ?(using: a compare option) a1 a2 =
  match using with
  | Some(Compare compare) -> if compare a1 a2 >= 0 then a1 else a2
  | _ -> if a1 >= a2 then a1 else a2

          
module List = struct

  include List
            
  type 'a t = 'a list [@@deriving eq, hash]

  let rec mem eq x = function
    | [] -> false
    | y::l when eq x y -> true
    | _::l -> mem eq x l

  let fold f seed l = List.fold_left (fun sofar elt -> f elt sofar) l seed

  let hash h = Hash.wrap1 hash_fold_t h
end
