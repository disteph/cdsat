(*******************************************************************)
(* Here are some tools for theory signatures and how to parse them *)
(*******************************************************************)

open Theories

let singleton = function
  | [a] -> a
  | l   -> raise (TypingError ("TypingError: expecting singleton but got list of length "^string_of_int (List.length l)))

(*******************************)
(* Standard multiary functions *)
(*******************************)

(* r_assoc is for right-associative symbols *)

let r_assoc aux sym l =
  let rec polish l =
    try [singleton (aux sym l)]
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

let rec l_assoc aux sym l =
  match aux sym l with
  | [a] -> a
  | l' when List.length l' < List.length l -> l_assoc aux sym l'
  | _   -> raise (TypingError "TypingError: left associativity did not reduce arguments list length")

(* pairwise is for parwise symbols such as Neq *)

let rec pairwise au sym =
  let aux sym l = singleton (au sym l) in
  let rec pair_aux a = function
    | []     -> aux `True []
    | [a']   -> aux sym [a;a']
    | a'::l' -> aux `And [aux sym [a;a'];pair_aux a l']
  in
  function
  | [] | [_]-> aux `True []
  | [a;a']  -> aux sym [a;a']
  | a::l    -> aux `And [pair_aux a l;pairwise au sym l]
      

(******************************)
(* Generic parsing mechanisms *)
(******************************)

let rec mapdbl l1 l2 = match l1,l2 with
  | (a1::l1'),(a2::l2') -> let (l,l')= mapdbl l1' l2' in ((a1 a2)::l,l')
  | l,[] -> ([],l)
  | [],_ -> raise (TypingError "TypingError: not enough arguments for symbol's arity")

let symb arit multiary symb_i sym expsort l =
  let aux sym l =
    let (output,input) = arit sym in
    let (combo,rest) = mapdbl l input in
    let a expsort =
      if output = expsort then symb_i sym combo
      else raise (TypingError "TypingError: symbol's output sort does not match expected sort")
    in
    a::rest
  in
  try singleton (aux sym l) expsort
  with TypingError _ -> multiary aux sym l expsort
    
(* The function below produces the record of the interpretation
functions, given the parameters ts (provided by the theory's
signature: ts.sortParse / ts.symbParse transform a string as a sort /
a list of possible symbols, and ts.arity provides the arity of
symbols) and st (provided by the decision procedure: st.symb_i). *)

let interpret 
    (ts: ('sort,'symbol,'a) forParsing)
    (st: ('sort,'symbol,'t) structureType)
    =
  { sigsymb =
      (fun s expsort l ->
        let rec aux = function
          | sym::k ->
            (try symb ts.arity ts.multiary st.sigsymb_i sym expsort l
	     with TypingError msg -> 
               if !Flags.debug>0
               then Dump.msg (Some ("\nWarning: could not understand string "^s^" as a specific (well-typed) signature symbol (now trying other ones) because:\n"^msg^"\n")) None None;
	       aux k)
          | []   -> raise (TypingError ("TypingError: cannot understand string "^s^" as a (well-typed) signature symbol"))
        in aux (ts.symbParse s));

    decsymb  =
      (fun s expsort l (decsort,decarg)
      -> let arit _ = (ts.sortParse decsort,List.map ts.sortParse decarg) in
         let multiary aux sym l = singleton (aux sym l) in
         symb arit multiary (st.decsymb_i expsort) s expsort l);

    boundsymb =
      (fun db so expected -> 
        if ts.sortParse so = expected then st.boundsymb_i db expected
        else raise (TypingError ("TypingError: De Bruijn's index "^(string_of_int db)^" bound with sort"^so^" but expecting another sort")));
    
    quantif =
      (fun b l sf ->
        let rec aux sf = function
          | []     -> sf          
          | so::l' -> st.quantif_i b (ts.sortParse so) (aux sf l')
        in
        aux sf l)
          
  }
