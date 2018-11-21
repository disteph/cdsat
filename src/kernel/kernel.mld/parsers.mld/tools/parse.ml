open Parser
       
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


open Multiary

(* val multiary  : symbol -> (symbol -> 'a list -> 'a) -> ((('a list->'a list) -> 'a list -> 'a) option) *)
let multiary =
  let open Top.Symbols in
  function
  | And | Or | Plus | Times | ITE _ -> Some r_assoc
  | Eq _ | NEq _ -> Some pairwise
  | _ -> None

let symbol ~decsorts symb sorts =
  let open Top.Symbols in
  let open Top.Sorts in
  match symb, sorts with
  | "true", []    -> True
  | "false", []   -> False
  | "not", [Prop] -> Neg
  | "and", [Prop;Prop] -> And
  | "or", [Prop;Prop]  -> Or
  | "imp", [Prop;Prop] | "=>", [Prop;Prop] -> Imp
  | "xor", [Prop;Prop] -> Xor
  | "<=>", [Prop;Prop] -> Eq Top.Sorts.Prop
  | "=", [s1;s2] | "==", [s1;s2] | "eq", [s1;s2]
    when Top.Sorts.equal s1 s2 -> Eq s1
  | "!=", [s1;s2] | "<>", [s1;s2] | "neq", [s1;s2] | "distinct", [s1;s2]
    when Top.Sorts.equal s1 s2 -> NEq s1
  | "ite", [Prop;s1;s2]
    when Top.Sorts.equal s1 s2 -> ITE s1
  | "+", [Rat;Rat] -> Plus
  | "-", [Rat;Rat] -> Minus
  | "-", [Rat]     -> Op
  | "*", [Rat;Rat] -> Times
  | "/", [Rat;Rat] -> Divide
  | ">", [Rat;Rat] -> Gt
  | "<", [Rat;Rat] -> Lt
  | ">=", [Rat;Rat] -> Ge 
  | "<=", [Rat;Rat] -> Le
  | "select", [Array(i1,v1);i2] when Top.Sorts.equal i1 i2 -> Select{indices=i1;values=v1}
  | "store",  [Array(i1,v1);i2;v2]
     when Top.Sorts.equal i1 i2 && Top.Sorts.equal v1 v2 -> Store{indices=i1;values=v1}
  | s, [] -> (try CstRat(Q.of_string s) with _ ->
      raise (ParsingError("Cannot understand symbol function "^s^" (no argument)")))
  | s, _ ->
    let sorts = List.fold (fun s sofar -> sofar^(Top.Sorts.show s)) sorts "" in
    raise (ParsingError("Cannot understand symbol function "^s^" with argument sorts "^sorts))
