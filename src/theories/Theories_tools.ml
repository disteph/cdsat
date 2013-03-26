open Theories
open Lib.Sums

(******************************)
(* Generic parsing mechanisms *)

exception TypingError of string

let rec extract_first_n = function
  | 0          -> (function l -> [],l)
  | i when i>0 -> (function l ->
		     let (a,b) = extract_first_n (i-1) (List.tl l) in
		       (List.hd l)::a, b)
  | i          -> raise (TypingError "Trying to extract a negative number of elements from list")


let symbmodel_aux arit symb_i sym expsort l =
  let (output,input,multiary) = arit sym in
    if output<> expsort 
    then raise (TypingError "TypingError: symbol's output sort does not match expected sort")
    else
      let arity = List.length input in
      let liftl b = function
	| (A a')                 -> a' b
	| (F(a',so)) when so = b -> a'
	| _         -> raise (TypingError "TypingError: stack of argument contains an evaluated argument whose type is not the expected one")
      in
      let f k = symb_i sym (List.map2 liftl input k) in
      let rec polish l = 	
	if      (List.length l = arity)&&(arity<=1)
	then f l
	else if (List.length l >= arity)&&(arity>1)
	then
	  let (a,l') = extract_first_n arity l in
	    polish (F(f a,output)::l')
	else match multiary,l with
	  | None,(F(a,o)::[]) when o=output -> a
	  | None,((A a)::[]) -> a output
	  | Some d,l when List.length l = arity-1
	      -> (match arit d with
		    | (o,[],_) ->
			f (List.fold_right (fun a l'-> a::l') l (F(symb_i d [],o)::[]))
		    | _ -> raise (TypingError "TypingError: Neutral element for operation is given arguments")
		 )
	  | None,_  -> raise (TypingError ("TypingError: multiary problem, no neutral element defined, list of length "^string_of_int(List.length l)))
	  | _,_  -> raise (TypingError ("TypingError: multiary problem, neutral element defined, list of length "
					^string_of_int(List.length l)
					^", expected "
					^string_of_int(arity)))
      in
      let rec polish2 multiary l = 	
	match multiary,l with
	  | _,_ when List.length l = arity 
	      -> [F(f l,output)]
	  | _,a::l' when (List.length l > arity)&&(arity>1)
	      -> polish2 None (a::(polish2 multiary l'))
	  | Some d,l when List.length l = arity-1
	      -> (match arit d with
		    | (o,[],_) ->
			polish2 None (List.fold_right (fun a l'-> a::l') l (F(symb_i d [],o)::[]))
		    | _ -> raise (TypingError "TypingError: Neutral element for operation is given arguments")
		 )
	  | _,_ -> l
      in
	match polish2 multiary (List.map (fun a-> A a) l) with
	  | [F(a,o)] when o=output -> a
	  | [A a] -> a output
	  | _ -> raise (TypingError ("TypingError: multiary problem"))
	(* polish (List.map (fun a-> A a) l) *)

let symbmodel symbParse arit symb_i s expsort l =
  let rec aux = function
    | sym::k -> (try symbmodel_aux arit symb_i sym expsort l
		 with TypingError msg 
		     -> (if !Flags.debug>0 then print_endline msg;
			 aux k))
    | []   -> raise (TypingError ("Cannot understand string "^s^" as a (well-typed) symbol"))
  in aux (symbParse s)


type ('sort,'t) interpretType = 
    { symbmodel : string -> 'sort -> ('sort->'t)list -> 't ;
      varmodel  : string -> 'sort -> (string option) -> 't}

let interpret 
    (ts: ('sort,'symbol) forParsing)
    (st: ('sort,'symbol,'t) structureType)
    =
  { symbmodel = symbmodel ts.symbParse ts.arity st.symb_i ;
    varmodel  = fun s expsort decsort
      -> match decsort with
	| Some so when (try (ts.sortParse so)<>expsort with _ -> true)
	    -> raise (TypingError ("Declared sort "^so^" for variable "^s^" not matching expected one"))
	| _ -> st.var_i s expsort
  }


open ThSig_register

module PropStructure(F:Kernel.Interfaces.PrintableFormulaType) = struct

  type 'a ite = 
    | Leaf of 'a
    | INode of F.t*('a ite)*('a ite)

  let rec map_aux f = function
    | Leaf a'      -> f a'
    | INode(c,d,e) -> INode(c,map_aux f d,map_aux f e)

  let map f    = map_aux (fun x -> Leaf(f x))

  let mmap f a = map_aux (fun y -> map (fun x->f x y) a)

  let rec bool_simpl = function
    | Leaf a      -> a
    | INode(c,d,e)-> F.orN(F.andP(c,bool_simpl d),F.andP(F.negation c,bool_simpl e))

  let symb_i (symb: [> PropSig.symbol]) (formulalist:F.t list) = 
    match symb, formulalist with
      | `True,[]      -> F.trueP
      | `False,[]     -> F.falseN
      | `Neg,[a]      -> F.negation a
      | `And,[a;b]    -> F.andP(a,b)
      | `Or,[a;b]     -> F.orN(a,b)
      | `Imp,[a;b]    -> F.orN(F.negation a,b)
      | `Xor,[a;b]    -> F.andP(F.orN(a,b),F.orN(F.negation a,F.negation b))
      | `EqProp,[a;b] -> F.andP(F.orN(F.negation a,b),F.orN(F.negation b,a))
      | `NEqProp,[a;b] -> F.orN(F.andP(a,F.negation b),F.andP(b,F.negation a))
      | `ITEProp,[a;b;c] -> bool_simpl(INode(a,Leaf b,Leaf c))
      | _             -> raise (TypingError "Not the right number of arguments")

end
