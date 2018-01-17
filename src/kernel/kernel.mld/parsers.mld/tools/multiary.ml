(*******************************)
(* Standard multiary functions *)
(*******************************)

exception MultiaryError of string

let singleton = function
  | [a] -> a
  | l   -> raise (MultiaryError ("MultiaryError: expecting singleton but got list of length "^string_of_int (List.length l)))

type 'a multiary = (Top.Symbols.t -> 'a list -> 'a list) -> Top.Symbols.t -> 'a list -> 'a

(* r_assoc is for right-associative symbols *)

let r_assoc interp sym l =
  let rec polish l =
    try [singleton (interp sym l)]
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

let rec l_assoc interp sym l =
  match interp sym l with
  | [a] -> a
  | l' when List.length l' < List.length l -> l_assoc interp sym l'
  | _   -> raise (MultiaryError "MultiaryError: left associativity did not reduce arguments list length")

(* pairwise is for parwise symbols such as Eq and Neq *)

let pairwise interp sym l =
  let as_singleton a a' = singleton (interp sym [a;a']) in
  let open Top in
  let rec pair_aux a = function
    | []     -> singleton(interp Symbols.True [])
    | [a']   -> as_singleton a a'
    | a'::l' -> singleton(interp Symbols.And [as_singleton a a';pair_aux a l'])
  in
  let rec aux = function
    | [] | [_]-> singleton(interp Symbols.True [])
    | [a;a']  -> as_singleton a a'
    | a::l    -> singleton(interp Symbols.And [pair_aux a l; aux l])
  in
  aux l
