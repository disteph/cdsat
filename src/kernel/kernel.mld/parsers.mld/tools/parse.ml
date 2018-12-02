open Top
open Terms
open Parser
open Multiary

       
let latexescaped = function
  | '%' | '{' | '}' as c -> "\\"^Char.escaped c
  | c -> Char.escaped c

let rec sort ~decsorts (Sort(s,l)) =
  let open Top.Sorts in
  match s with
  | "Bool" | "bool" | "Prop" | "prop"
    -> (match l with
    | [] -> Prop
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | "Rat"  | "Real" | "Int"
    -> (match l with
    | [] -> Rat
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | "Array"
    -> (match l with
    | s1::s2::[] -> let so1,so2 = sort decsorts s1, sort decsorts s2
                    in Array(so1,so2)
    | _  -> raise (ParsingError("Sort "^s^" is applied to "^string_of_int(List.length l)^" arguments")))
  | s when List.mem [%eq:string] s decsorts -> User s
  | s -> raise (ParsingError("Cannot understand "^s^" as a sort: not declared"))

       
let symbol ~decsorts bc symb l =
  let lsorts = List.map Terms.TermB.get_sort l in
  let open Symbols in
  let open Sorts in
  match symb, lsorts with
  | "true", _    -> exact bc True []
  | "false", _   -> exact bc False []
  | "not", _ -> exact bc Neg l
  | "and", _ -> trythese [exact bc True; singleton; r_assoc bc And] l
  | "or", _  -> trythese [exact bc False; singleton; r_assoc bc Or] l
  | "imp", _ | "=>", _ -> trythese [singleton; r_assoc bc Imp] l
  | "xor", _ -> exact bc Xor l
  | "<=>", _ -> pairwise bc (Eq Prop) l
  | "=", s1::_
  | "==", s1::_
  | "eq", s1::_ -> pairwise bc (Eq s1) l
  | "!=", s1::_
  | "<>", s1::_
  | "neq", s1::_
  | "distinct", s1::_  -> pairwise bc (NEq s1) l
  | "ite", Prop::s1::_ -> r_assoc bc (ITE s1) l
  | "+", [Rat;Rat] -> r_assoc bc Plus l
  | "-", [Rat;Rat] -> exact bc Minus l
  | "-", [Rat]     -> exact bc Op l
  | "*", [Rat;Rat] -> r_assoc bc Times l
  | "/", [Rat;Rat] -> exact bc Divide l
  | ">", [Rat;Rat] -> exact bc Gt l
  | "<", [Rat;Rat] -> exact bc Lt l
  | ">=", [Rat;Rat] -> exact bc Ge l
  | "<=", [Rat;Rat] -> exact bc Le l
  | "select", [Array(i1,v1);i2] when Top.Sorts.equal i1 i2
    -> exact bc (Select{indices=i1;values=v1}) l
  | "store",  [Array(i1,v1);i2;v2]
    when Top.Sorts.equal i1 i2 && Top.Sorts.equal v1 v2
    -> exact bc (Store{indices=i1;values=v1}) l
  | s, [] -> (try exact bc (CstRat(Q.of_string s)) l with _ ->
      raise (ParsingError("Cannot understand symbol function "^s^" (no argument)")))
  | s, _ ->
    let sorts = List.fold (fun s sofar -> sofar^(Top.Sorts.show s)) lsorts "" in
    raise (ParsingError("Cannot understand symbol function "^s^" with argument sorts "^sorts))
