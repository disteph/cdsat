(* SMTLib2 parser *)

open SMTLib2_tools
open Smtlib2_ast
open Parser

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

let getsort = function
  | SortIdentifier(_,IdSymbol(_,symb)) -> getsymbSymbol symb
  | _ -> failwith("Could not parse sort")

let getsortedvar = function
  | SortedVarSymSort(_,symb,sort) -> (getsymbSymbol symb,getsort sort)

type 'a getsymbType =
| AppliedSymbol of string*(term list)*('a environment)
| Quantif of bool*((string*string) list)*term

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
  | TermSpecConst(_,a)            -> AppliedSymbol(getconst a,[],env)
  | TermForAllTerm(_,(_, l),t)    -> Quantif(true,List.map getsortedvar l,t)
  | TermExistsTerm(_,(_, l),t)    -> Quantif(false,List.map getsortedvar l,t)
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
	  | None,_       -> AppliedSymbol(s,tl,env)
	  | Some(t,e),[] -> getsymb e t
	  | _,_          -> raise (ParsingError "Higher-order definition")
      end
  | IdUnderscoreSymNum(_,symb,_)-> raise (ParsingError "Not sure")


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

let rec list_index a accu = function
  | [] -> None
  | (b,so)::l when a=b -> Some (accu,so)
  | _::l -> list_index a (accu+1) l

let parse (type t) names (theory,satprov,status,declared,formulalist) i = 

  let module I = (val i: TheoryParsing.InterpretType with type t=t) in
  
  let rec transformTermBase env boundvarlist s l =
    match list_index s 0 boundvarlist,l with
    | Some(db,so),[]  -> I.boundsymb db so
    | Some(_,_) , _   -> raise (ParsingError "Higher-order quantification")
    | None,_          ->
      let l' = List.map (transformTerm env boundvarlist) l in
      if SM.mem s declared
      then I.decsymb s (SM.find s declared) l'
      else I.sigsymb s l'
      
  and transformTerm env boundvarlist t =
    match getsymb env t with
    | AppliedSymbol(s,l,newenv) -> transformTermBase newenv boundvarlist s l
    | Quantif(b,l,t') -> I.quantif b (List.map snd l) (transformTerm env (List.append l boundvarlist) t')
      
  in

    (match theory with
       | Some th when not(List.mem th names) ->
	   print_endline("WARNING: Input mentions theory "^th^" that is not among the theory's names")
       | _ ->());
    match formulalist with
      | []-> (None,status)
      | _ -> 
	let formula = transformTermBase EmptyEnv [] "and" formulalist in
	let fformula = if satprov then formula else I.sigsymb "not" [formula] in
	(Some fformula,status)
