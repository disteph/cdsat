(**************************************************************)
(* This is the register of all theories' signatures in Psyche *)
(**************************************************************)

open Theories

module PropSig = struct
  type sort   = [ `Prop ]
  type symbol = [ `True | `False | `Neg | `And | `Or | `Imp | `Xor | `EqProp | `NEqProp | `ITEProp ]

  let forParser =
    {names   = ["prop";"empty"];
     prop    = `Prop }

  let forParsing =
    { arity = 
        (function
        | `True    -> (`Prop, [])
	| `False   -> (`Prop, [])
	| `Neg     -> (`Prop, [`Prop])
	| `And     -> (`Prop, [`Prop ;`Prop])
	| `Or      -> (`Prop, [`Prop ;`Prop])
	| `Imp     -> (`Prop, [`Prop ;`Prop])
	| `Xor     -> (`Prop, [`Prop ;`Prop])
	| `EqProp  -> (`Prop, [`Prop ;`Prop])
	| `NEqProp -> (`Prop, [`Prop ;`Prop])
	| `ITEProp -> (`Prop, [`Prop;`Prop;`Prop])
	| _        -> raise (Match_failure("Could not find connective ",0,0)));
      multiary  = ThSig_tools.r_assoc;
      sortParse = 
        (function
        | "Bool" | "bool" | "Prop" | "prop" -> `Prop
	| s       -> raise (TypingError("TypingError: cannot understand sort "^s)));
      symbParse =
        (function
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
    {names   = ["lra";"qflra"; "QF_LRA"];
     prop    = `Prop }

  let forParsing =
    { arity = 
        (function
	| `CstRat i -> (`Rat, [])
	| `Plus  -> (`Rat, [`Rat ;`Rat])
	| `Minus -> (`Rat, [`Rat ;`Rat])
	| `Times -> (`Rat, [`Rat ;`Rat])
	| `Divide-> (`Rat, [`Rat ;`Rat])
	| `Op    -> (`Rat, [`Rat])
	| `ITERat-> (`Rat, [`Prop;`Rat;`Rat])
	| `EqRat -> (`Prop, [`Rat ;`Rat])
	| `NEqRat-> (`Prop, [`Rat ;`Rat])
	| `Ge    -> (`Prop, [`Rat ;`Rat])
	| `Gt    -> (`Prop, [`Rat ;`Rat])
	| `Le    -> (`Prop, [`Rat ;`Rat])
	| `Lt    -> (`Prop, [`Rat ;`Rat])
	| s      -> PropSig.forParsing.arity s);
      multiary  =
        (fun aux s -> match s with
        | `NEqRat | `EqRat -> ThSig_tools.pairwise aux s
        | _ -> ThSig_tools.r_assoc aux s);
      sortParse = 
        (function
	| "Rat"  -> `Rat
	| "Real" -> `Rat
	| "Int"  -> `Rat
	| s      -> PropSig.forParsing.sortParse s);
      symbParse = 
        (function s -> 
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

module CCemptySig = struct
  type sort   = [ `Prop | `Term ]
  type symbol = [ PropSig.symbol | `EqTerm | `NEqTerm ]

  let forParser =
    {names   = ["emptyCC";"QF_UF"];
     prop    = `Prop }

  let forParsing =
    { arity = 
        (function
        | `EqTerm -> (`Prop, [`Term ;`Term])
        | `NEqTerm-> (`Prop, [`Term ;`Term])
        | s       -> PropSig.forParsing.arity s);
      multiary  =
        (fun aux s -> match s with
        | `NEqTerm | `EqTerm -> ThSig_tools.pairwise aux s
        | _ -> ThSig_tools.r_assoc aux s);
      sortParse = 
        (function s ->
          (try PropSig.forParsing.sortParse s with _ -> `Term));
      symbParse = 
        (function s -> 
	  let l = match s with
	    | "="  | "==" | "eq" ->[`EqTerm]
	    | "!=" | "<>" | "neq" | "distinct" ->[`NEqTerm]
	    | _    -> []
	  in
	  List.rev_append l (PropSig.forParsing.symbParse s))
    }

end

module FOSig = struct
  type sort   = [ `Prop | `Term ]
  type symbol = PropSig.symbol

  let forParser =
    {names   = ["FO"];
     prop    = `Prop }

  let forParsing =
    { arity     = PropSig.forParsing.arity;
      multiary  = ThSig_tools.r_assoc;
      sortParse = (function s ->
        (try PropSig.forParsing.sortParse s with _ -> `Term));
      symbParse = PropSig.forParsing.symbParse
    }

end
