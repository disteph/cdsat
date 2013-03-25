open Theories

(* This is the register of all theories' signatures in Psyche *)

module PropSig = struct
  type sort   = [ `Prop ]
  type symbol = [ `True | `False | `Neg | `And | `Or | `Imp | `Xor | `EqProp | `NEqProp | `ITEProp ]

  let forParser =
    {names   = ["prop";"empty"];
     prop    = `Prop }

  let forParsing =
    { arity = (function
		 | `True -> (`Prop, [], None)
		 | `False-> (`Prop, [], None)
		 | `Neg  -> (`Prop, [`Prop], None)
		 | `And  -> (`Prop, [`Prop ;`Prop],None)
		 | `Or   -> (`Prop, [`Prop ;`Prop],None)
		 | `Imp  -> (`Prop, [`Prop ;`Prop],None)
		 | `Xor  -> (`Prop, [`Prop ;`Prop],None)
		 | `EqProp  -> (`Prop, [`Prop ;`Prop],None)
		 | `NEqProp -> (`Prop, [`Prop ;`Prop],None)
		 | `ITEProp -> (`Prop, [`Prop;`Prop;`Prop],None)
		 | _     -> raise (Match_failure("Could not find connective ",0,0)));
      sortParse = (function
		     | "Bool"  -> `Prop
		     | "Prop"  -> `Prop
		     | s       -> failwith("Cannot understand sort "^s));
      symbParse = (function
		     | "true"  -> [`True]
		     | "false" -> [`False]
		     | "not"   -> [`Neg]
		     | "and"   -> [`And]
		     | "or"    -> [`Or]
		     | "imp" | "=>" -> [`Imp]
		     | "xor"   -> [`Xor]
		     | "=" | "<=>"  -> [`EqProp]
		     | "distinct"   -> [`NEqProp]
		     | "ite"   -> [`ITEProp]
		     | _       -> [])
    }

end


module LRASig = struct
  type sort   = [ `Prop | `Rat ]
  type symbol = [ PropSig.symbol 
  | `CstRat of Num.num
  | `Ge | `Le | `Gt | `Lt | `EqRat | `NEqRat
  | `Plus | `Minus | `Times | `Divide | `Op | `ITERat]

  let forParser =
    {names   = ["lra";"qflra"];
     prop    = `Prop }

  let forParsing =
    { arity = (function
		 | `CstRat i -> (`Rat, [], None)
		 | `Plus  -> (`Rat, [`Rat ;`Rat], None)
		 | `Minus -> (`Rat, [`Rat ;`Rat], None)
		 | `Times -> (`Rat, [`Rat ;`Rat], None)
		 | `Divide-> (`Rat, [`Rat ;`Rat], None)
		 | `Op    -> (`Rat, [`Rat], None)
		 | `ITERat-> (`Rat, [`Prop;`Rat;`Rat], None)
		 | `EqRat -> (`Prop, [`Rat ;`Rat], None)
		 | `NEqRat-> (`Prop, [`Rat ;`Rat], None)
		 | `Ge    -> (`Prop, [`Rat ;`Rat], None)
		 | `Gt    -> (`Prop, [`Rat ;`Rat], None)
		 | `Le    -> (`Prop, [`Rat ;`Rat], None)
		 | `Lt    -> (`Prop, [`Rat ;`Rat], None)
		 | s      -> PropSig.forParsing.arity s);
      sortParse = (function
		     | "Rat"  -> `Rat
		     | "Real" -> `Rat
		     | s      -> PropSig.forParsing.sortParse s);
      symbParse = (function s -> 
		     let l = match s with
		       | "+" -> [`Plus]
		       | "-" -> [`Minus;`Op]
		       | "*" -> [`Times]
		       | "/" -> [`Divide]
		       | ">" -> [`Gt]
		       | "<" -> [`Lt]
		       | ">=" ->[`Ge]
		       | "<=" ->[`Le]
		       | "=" | "==" | "eq" ->[`EqRat]
		       | "!=" | "<>" | "neq" | "distinct" ->[`NEqRat]
		       | "ite"->[`ITERat]
		       | s    -> (try [`CstRat(Num.num_of_string s)] with _ -> [])
		     in
		       List.rev_append l (PropSig.forParsing.symbParse s))
    }

end
