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

(* val multiary  : symbol -> ((('a list->'a list) -> 'a list -> 'a) option) *)
let multiary =
  let open Top.Symbols in
  function
  | And | Or | Plus | Times -> Some r_assoc
  (*   | NEqRat | EqRat -> None (\* ThSig_tools.pairwise *\) *)
  | _ -> None

let symbol ~decsorts = 
  let open Top in
  let open Symbols in
  let allsorts = Sorts.allsorts decsorts in function
    | "true"  -> [True]
    | "false" -> [False]
    | "not"   -> [Neg]
    | "and"   -> [And]
    | "or"    -> [Or]
    | "imp" | "=>" -> [Imp]
    | "xor"   -> [Xor]
    | "<=>"   -> [Eq Top.Sorts.Prop]
    | "=" | "==" | "eq"                -> List.map (fun so -> Eq so) allsorts
    | "!=" | "<>" | "neq" | "distinct" -> List.map (fun so -> NEq so) allsorts
    | "ite"                            -> List.map (fun so -> ITE so) allsorts
    | "+" -> [Plus]
    | "-" -> [Minus;Op]
    | "*" -> [Times]
    | "/" -> [Divide]
    | ">" -> [Gt]
    | "<" -> [Lt]
    | ">=" ->[Ge]
    | "<=" ->[Le]
    | "select" -> let aux ind = List.map (fun so -> Select(ind,so)) allsorts
                  in List.fold (fun ind accu -> (aux ind)@accu) allsorts []
    | "store"  -> let aux ind = List.map (fun so -> Store(ind,so)) allsorts
                  in List.fold (fun ind accu -> (aux ind)@accu) allsorts []
    | s    -> try [CstRat(Q.of_string s)] with _ -> []
