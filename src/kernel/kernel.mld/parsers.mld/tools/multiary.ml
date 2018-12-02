(*******************************)
(* Standard multiary functions *)
(*******************************)

open Top
open Terms
    
exception TypingError of string

let singleton = function
  | [a] -> a
  | l   -> raise (TypingError ("TypingError: expecting singleton but got list of length "^string_of_int (List.length l)))


type multiary =
  (Top.Symbols.t -> TermB.t list -> TermB.t) ->
  Top.Symbols.t -> TermB.t list -> TermB.t

(* r_assoc is for right-associative symbols *)

let r_assoc interp l =
  let rec polish l =
    try [singleton (interp l)]
    with TypingError _ ->
      match l with
      | a::l' -> let l'' = polish l' in
                 if List.length l'' < List.length l'
                 then polish (a::l'')
                 else l
      | _     -> l
  in
  singleton (polish l)

(* l_assoc is for left-associative symbols *)

let rec l_assoc interp l =
  match interp l with
  | [a] -> a
  | l' when List.length l' < List.length l -> l_assoc interp l'
  | _   -> raise (TypingError "TypingError: left associativity did not reduce arguments list length")

(* pairwise is for parwise symbols such as Eq and Neq *)

let pairwise interp sym l =
  let as_singleton a a' = interp sym [a;a'] in
  let open Top in
  let rec pair_aux a = function
    | []     -> interp Symbols.True []
    | [a']   -> as_singleton a a'
    | a'::l' -> interp Symbols.And [as_singleton a a';pair_aux a l']
  in
  let rec aux = function
    | [] | [_]-> interp Symbols.True []
    | [a;a']  -> as_singleton a a'
    | a::l    -> interp Symbols.And [pair_aux a l; aux l]
  in
  aux l

let rec mapdbl sorts args = match sorts,args with
  | (s1::l1'),(t::l2') ->
    let s2 = TermB.get_sort t in
    if Sorts.equal s1 s2
    then let l,rest= mapdbl l1' l2' in t::l, rest
    else raise (TypingError ("TypingError: expected "^Sorts.show s1^" for "^TermB.show t^", got "^Sorts.show s2))
  | [],l -> [],l
  | _,[] ->
    raise (TypingError "TypingError: not enough arguments for symbol's arity")

let trythis bc sym args =
  let _,input = Symbols.arity sym in
  let combo,rest   = mapdbl input args in
  (bc sym combo)::rest

let exact bc sym args = singleton(trythis bc sym args)
let r_assoc bc sym args = r_assoc (trythis bc sym) args
let l_assoc bc sym args = l_assoc (trythis bc sym) args

let rec trythese syms args = match syms with
  | [] -> raise (TypingError ("TypingError: nothing worked"))
  | a::syms ->
    try a args
    with TypingError _ -> trythese syms args
