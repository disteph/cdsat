(* SMTLib2 parser *)

open SMTLib2_tools
open Smtlib2_ast
open Parsers

let name = "SMTLib2"

type 'a environment =
  | EmptyEnv
  | ConsEnv of 'a*('a environment)*('a environment)

let rec search symb = function
  | EmptyEnv                       -> None
  | ConsEnv((s,t),e,l) when s=symb -> Some(t,e)
  | ConsEnv(_,_,l)                 -> search symb l

let getconst = function
  | SpecConstsDec   (_,s) -> s
  | SpecConstNum    (_,s) -> s
  | SpecConstString (_,s) -> s
  | SpecConstsHex   (_,s) -> s
  | SpecConstsBinary(_,s) -> s

let getsymbSymbol = function
  | Symbol(_,symb)      -> symb 
  | SymbolWithOr(_,symb)-> symb

let rec getsymb env = function
  | TermLetTerm(_,(_,l),t) -> 
      let newenv = 
	List.fold_left 
	  (fun locenv (VarBindingSymTerm(_,sym,t)) -> ConsEnv((getsymbSymbol sym,t),env,locenv))
	  env
	  l in
	getsymb newenv t
  | TermQualIdentifier(_,symb)    -> getsymbQualId env [] symb
  | TermQualIdTerm(_,symb,(_,tl)) -> getsymbQualId env tl symb
  | TermSpecConst(_,a)            -> (getconst a,[],env)
  | _                             -> raise (ParsingError "Cannot understand term");
      
and getsymbQualId env tl = function
  | QualIdentifierId(_,symb)   -> getsymbIdSymbol env tl symb 
      (*	| QualIdentifierAs(_,symb,_) -> getsymbIdSymbol env tl symb *)
  | _ -> raise (ParsingError "Symbol was in fact sorted")

and getsymbIdSymbol env tl = function
  | IdSymbol(_,symb) ->
      begin
	let s = getsymbSymbol symb in
	match search s env,tl with
	  | None,_       -> (s,tl,env)
	  | Some(t,e),[] -> getsymb e t
	  | _,_          -> raise (ParsingError "Higher-order definition")
      end
  | IdUnderscoreSymNum(_,symb,_)-> raise (ParsingError "Not sure")

let getsort = function
  | SortIdentifier(_,IdSymbol(_,symb)) -> getsymbSymbol symb
  | _ -> failwith("Could not parse sort")

let getStatus = function
  | AttributeValSymbol(_,s) -> 
      begin
	match getsymbSymbol s with
	  | "unsat"      -> Some true
	  | "sat"        -> Some false 
	  | "provable"   -> Some true
	  | "unprovable" -> Some false 
	  | "unknown"    -> None
	  | _            -> print_endline("Status could not be parsed");None
      end
  | _ -> print_endline("Status could not be parsed");None

module SM = Map.Make(String)

type afterglance = (string option)*bool*(bool option)*((string * (string list)) SM.t)*(Smtlib2_ast.term list)

let transformCommand (theory,satprov,status,declared,formula) = function
  | CAssert(_,t)                 -> (theory,satprov,status,declared,t::formula)
  | CSetLogic(_,symbol)          ->
      begin
	match theory with
	  | Some _ -> print_endline("Trying to declare theory twice");(theory,satprov,status,declared,formula)
	  | None   ->(Some(getsymbSymbol symbol),satprov,status,declared,formula)
      end
  | CSetInfo(_,AttributeKeywordValue(_,":status",attribute)) ->
      begin
	match status with
	  | Some _ -> print_endline("Trying to declare status twice");(theory,satprov,None,SM.empty,[])
	  | None   ->(theory,satprov,getStatus attribute,declared,formula)
      end
  | CCheckSat _                  -> (theory,false,status,declared,formula)
  | CDeclareFun(_,symbol,(_,l),sort) ->
      (theory,satprov,status,SM.add (getsymbSymbol symbol) (getsort sort,List.map (fun s -> getsort s) l) declared,formula)
  | _                            -> (theory,satprov,status,declared,formula)

let transformCommands (Commands(_,(_,li))) =
  List.fold_left transformCommand (None,true,None,SM.empty,[]) li

let glance input =
  let lexbuf = Lexing.from_string input in
  let h = 
    try Smtlib2_parse.main Smtlib2_lex.token lexbuf 
    with
    | Parsing.Parse_error -> raise (ParsingError "Alt-Ergo's parser could not parse input (raised exception)")
    | Smtlib2_lex.LexingError msg -> raise (ParsingError ("Alt-Ergo's parser could not lex input: \""^msg^"\""))
  in
    match h with
      | None    -> raise (ParsingError "Alt-Ergo's parser could not parse input (returned None)")
      | Some ast-> transformCommands ast
	  
let guessThDecProc(theory,_,_,_,_) =  
  match theory with
    | None   -> ""
    | Some a -> a

let parse ts i (theory,satprov,status,declared,formulalist) = 

  (* i's functions may raise Theories.TypingError *)

  let open Theories in
  
  (* The following may raise Theories.TypingError *)

  let rec transformTermBase env expsort s l =
    let l' = List.map (transformTerm  env) l in
    if SM.mem s declared
    then i.decsymb s expsort l' (SM.find s declared)
    else i.sigsymb s expsort l'
        
  and transformTerm env t expsort =
    let (s,l,newenv) = getsymb env t in
      transformTermBase newenv expsort s l
  in

    (match theory with
       | Some th when not(List.mem th ts.names) ->
	   print_endline("WARNING: Input mentions theory "^th^" that is not among the theory's names")
       | _ ->());
    match formulalist with
      | []          -> (None,status)
      | _ -> 
	  let formula = transformTermBase EmptyEnv ts.prop "and" formulalist in
	  let fformula =
	    if satprov then formula
	    else 
	      i.sigsymb "not" ts.prop [fun so -> if so=ts.prop then formula else raise(ParsingError "\"Formula\" to find UNSAT is not of type `Prop!")]
	  in
	    (Some fformula,status)
