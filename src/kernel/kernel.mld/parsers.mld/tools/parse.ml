open Top
open Terms
open Parser
open TypingBase

       
let latexescaped = function
  | '%' | '{' | '}' as c -> "\\"^Char.escaped c
  | c -> Char.escaped c

let rec sort ~decsorts (Sort((s,param),l)) =
  let open Top.Sorts in
  match s,param,l with
  | "Bool",[],[] | "bool",[],[] | "Prop",[],[] | "prop",[],[]
    -> Prop
  | "Rat",[],[]  | "Real",[],[] | "Int",[],[]
    -> Rat
  | "Array",[], [s1;s2]
    -> let so1,so2 = sort decsorts s1, sort decsorts s2
    in Array(so1,so2)
  | "BitVec",[width],[]
    -> BV(int_of_string width)
  | s,[],[] when List.mem [%eq:string] s decsorts -> User s
  | _ -> raise (ParsingError("Cannot understand "^s^"("^List.show Stringhashed.pp param^") as a sort"))

       
let symbol ~decsorts bc (symb,param) l =
  let lsorts = List.map Terms.TermB.get_sort l in
  let open Symbols in
  let open Sorts in
  match symb, param, lsorts with
  | "true", [], _    -> exact bc True []
  | "false", [], _   -> exact bc False []
  | "not", [], _ -> exact bc Neg l
  | "and", [], _ -> trythese [exact bc True; singleton; r_assoc bc And] l
  | "or", [], _  -> trythese [exact bc False; singleton; r_assoc bc Or] l
  | "imp", [], _
  | "=>", [], _ -> trythese [singleton; r_assoc bc Imp] l
  | "xor", [], _ -> exact bc Xor l
  | "<=>", [], _ -> pairwise bc (Eq Prop) l
  | "=", [], s1::_
  | "==", [], s1::_
  | "eq", [], s1::_ -> pairwise bc (Eq s1) l
  | "!=", [], s1::_
  | "<>", [], s1::_
  | "neq", [], s1::_
  | "distinct", [], s1::_  -> pairwise bc (NEq s1) l
  | "ite", [], Prop::s1::_ -> r_assoc bc (ITE s1) l
  | "+", [], [Rat;Rat] -> r_assoc bc Plus l
  | "-", [], [Rat;Rat] -> exact bc Minus l
  | "-", [], [Rat]     -> exact bc Op l
  | "*", [], [Rat;Rat] -> r_assoc bc Times l
  | "/", [], [Rat;Rat] -> exact bc Divide l
  | ">", [], [Rat;Rat] -> exact bc Gt l
  | "<", [], [Rat;Rat] -> exact bc Lt l
  | ">=", [], [Rat;Rat] -> exact bc Ge l
  | "<=", [], [Rat;Rat] -> exact bc Le l

  | "select", [], [Array(i1,v1);i2] when Sorts.equal i1 i2
    -> exact bc (Select{indices=i1;values=v1}) l

  | "store", [],  [Array(i1,v1);i2;v2]
    when Sorts.equal i1 i2 && Sorts.equal v1 v2
    -> exact bc (Store{indices=i1;values=v1}) l

  | "extract", [his;los], [BV length] 
    -> let hi,lo = int_of_string his, int_of_string los in
    if hi >= lo then exact bc (BVextract{ hi; lo; length }) l
    else raise (ParsingError("extract with high="^his^" lower than low="^los))

  | "concat", [], [BV i; BV j] -> exact bc (BVconc(i,j)) l
  | "bvnot", [], [BV i] -> exact bc (BVnot i) l
  | "bvneg", [], [BV i] -> exact bc (BVneg i) l
  | "bvand", [], [BV i; BV j] when i=j -> l_assoc bc (BVand i) l
  | "bvor" , [], [BV i; BV j] when i=j -> l_assoc bc (BVor i) l
  | "bvxor", [], [BV i; BV j] when i=j -> exact bc (BVxor i) l
  | "bvadd", [], [BV i; BV j] when i=j -> l_assoc bc (BVadd i) l
  | "bvmul", [], [BV i; BV j] when i=j -> l_assoc bc (BVmul i) l
  | "bvudiv", [], [BV i; BV j] when i=j -> exact bc (BVudiv i) l
  | "bvurem", [], [BV i; BV j] when i=j -> exact bc (BVurem i) l
  | "bvshl", [], [BV i; BV j] when i=j -> exact bc (BVshl i) l
  | "bvshr", [], [BV i; BV j] when i=j -> exact bc (BVshr i) l
  | "bvult", [], [BV i; BV j] when i=j -> exact bc (BVult i) l

  | s, [], [] ->
    begin
      try exact bc (CstRat(Q.of_string s)) l with _ ->
      try 
        let sub = String.sub s 2 ((String.length s)-2) in
        exact bc (BVcst(Bv_value.constb sub)) l
      with _ ->
        raise (ParsingError("Cannot understand symbol function "^s^" (no argument)"))
    end

  | s, _, _ ->
    let sorts = List.show Sorts.pp lsorts in
    raise (ParsingError("Cannot understand symbol function "^s^" with argument sorts "^sorts))
