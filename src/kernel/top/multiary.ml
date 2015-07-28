(*******************************)
(* Standard multiary functions *)
(*******************************)

exception MultiaryError of string

let singleton = function
  | [a] -> a
  | l   -> raise (MultiaryError ("MultiaryError: expecting singleton but got list of length "^string_of_int (List.length l)))

(* r_assoc is for right-associative symbols *)

let r_assoc interpret l =
  let rec polish l =
    try [singleton (interpret l)]
    with MultiaryError _ ->
      match l with
      | a::l' -> let l'' = polish l' in
                 if List.length l'' < List.length l'
                 then polish (a::l'')
                 else l
      | _     -> l
  in
  singleton (polish l)

(* l_assoc is for left-associative symbols *)

let rec l_assoc interpret l =
  match interpret l with
  | [a] -> a
  | l' when List.length l' < List.length l -> l_assoc interpret l'
  | _   -> raise (MultiaryError "MultiaryError: left associativity did not reduce arguments list length")

(* pairwise is for parwise symbols such as Eq and Neq *)

let pairwise true_i and_i interpret =
  let as_singleton a a' = singleton (interpret [a;a']) in
  let rec pair_aux a = function
    | []     -> true_i
    | [a']   -> as_singleton a a'
    | a'::l' -> and_i [as_singleton a a';pair_aux a l']
  in
  let rec aux = function
    | [] | [_]-> true_i
    | [a;a']  -> as_singleton a a'
    | a::l    -> and_i [pair_aux a l; aux l]
  in
  aux
