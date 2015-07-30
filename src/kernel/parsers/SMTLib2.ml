(* SMTLib2 parser *)

open SMTLib2_tools
open Smtlib2_ast
open Top.Parser

let name = "SMTLib2"

type 'a environment =
  | EmptyEnv
  | ConsEnv of 'a*('a environment)*('a environment)

let rec search symb = function
  | EmptyEnv                       -> None
  | ConsEnv((s,t),e,l) when s=symb -> Some(t,e)
  | ConsEnv(_,_,l)                 -> search symb l

let getconst = function
  | SpecConstsDec   (_,s)
  | SpecConstNum    (_,s)
  | SpecConstString (_,s)
  | SpecConstsHex   (_,s)
  | SpecConstsBinary(_,s) -> s

let getsymbSymbol = function
  | Symbol(_,symb)
  | SymbolWithOr(_,symb)-> symb

let getsort = function
  | SortIdentifier(_,IdSymbol(_,symb)) -> getsymbSymbol symb
  | _ -> raise (ParsingError "Could not parse sort")

let getsortedvar (SortedVarSymSort(_,symb,sort)) =
  getsymbSymbol symb, getsort sort

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
	  | _,_          -> raise (ParsingError("Higher-order definition for "^s))
      end
  | IdUnderscoreSymNum(_,symb,_)-> raise (ParsingError("Not sure about "^getsymbSymbol symb))


let getStatus = function
  | AttributeValSymbol(_,s) -> 
      begin
	match getsymbSymbol s with
	  | "unsat"      -> Some true
	  | "sat"        -> Some false 
	  | "provable"   -> Some true
	  | "unprovable" -> Some false 
	  | "unknown"    -> None
	  | _            -> print_endline("Status could not be parsed:"^getsymbSymbol s);None
      end
  | _ -> print_endline "Status could not be parsed"; None

let string_of_stat = function
  | Some true  -> "provable/unsat"
  | Some false -> "unprovable/sat"
  | None       -> "unknown"

module SM = Map.Make(String)

type afterglance = 
  (string option)                   (* Theory, if specified *)
  *bool                             (* Whether we prove (true) or refute (false) *)
  *(bool option)                    (* The status of the problem (Some true) if provable/unsat, (Some false) if counter-model/sat *)
  *(unit SM.t)                      (* Declared sorts: set of strings *)
  *((string * (string list)) SM.t)  (* Declared symbols: strings mapped to their arity:
                                       a pair whose first component is the output sort, second component is the list of arguments' sorts *)
  *(Smtlib2_ast.term list)          (* The problem instance *)

let transformCommand (theory,satprov,status,decso,decsym,formula) = function
  | CAssert(_,t)                 -> theory,satprov,status,decso,decsym,t::formula
  | CSetLogic(_,symbol)          ->
    begin
      match theory with
      | Some prevtheory -> 
        begin
          print_endline("Trying to declare theory twice: was "^prevtheory
                        ^", now declared as "^getsymbSymbol symbol^". Keeping "^prevtheory);
          (theory,satprov,status,decso,decsym,formula)
        end
      | None   -> Some(getsymbSymbol symbol),satprov,status,decso,decsym,formula
    end
  | CSetInfo(_,AttributeKeywordValue(_,":status",attribute)) ->
    begin
      match status with
      | Some prevstatus -> 
        begin
          print_endline("Trying to declare status twice: was "^string_of_stat status
                        ^", now declared as "^string_of_stat (getStatus attribute)^". Keeping "^string_of_stat status);
          (theory,satprov,status,decso,decsym,formula)
        end
      | None  -> theory,satprov,getStatus attribute,decso,decsym,formula
    end
  | CCheckSat _                      -> theory,false,status,decso,decsym,formula
  | CDeclareSort(_,symbol,arity)     ->
    if int_of_string arity != 0
    then raise (ParsingError ("Trying to declare sort "^getsymbSymbol symbol^" with arity "^arity
                              ^", but Psyche can only handle sorts of arity 0"))
    else theory,satprov,status,SM.add (getsymbSymbol symbol) () decso,decsym,formula
  | CDeclareFun(_,symbol,(_,l),sort) ->
      theory,satprov,status,decso,SM.add (getsymbSymbol symbol) (getsort sort,List.map getsort l) decsym,formula
  | _                                -> theory,satprov,status,decso,decsym,formula

let transformCommands (Commands(_,(_,li))) =
  List.fold_left transformCommand (None,true,None,SM.empty,SM.empty,[]) li

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
	  
let guessThDecProc(theory,_,_,_,_,_) =  
  match theory with
    | None   -> None
    | Some a -> Some [a]

let rec list_index a accu = function
  | [] -> None
  | (b,so)::l when a=b -> Some (accu,so)
  | _::l -> list_index a (accu+1) l

let parse (type t) (theory,satprov,status,decso,decsym,formulalist) i =

  let module I = (val i (SM.fold (fun s () l -> s::l) decso []): InterpretType with type t=t) in
  
  let rec transformTermBase env boundvarlist s l =
    match list_index s 0 boundvarlist,l with
    | Some(db,so),[]  -> I.boundsymb db so
    | Some(_,_) , _   -> raise (ParsingError "Higher-order quantification")
    | None,_          ->
      let l' = List.map (transformTerm env boundvarlist) l in
      if SM.mem s decsym
      then I.decsymb s (SM.find s decsym) l'
      else I.sigsymb s l'
      
  and transformTerm env boundvarlist t =
    match getsymb env t with
    | AppliedSymbol(s,l,newenv) -> transformTermBase newenv boundvarlist s l
    | Quantif(b,l,t') -> I.quantif b (List.map snd l) (transformTerm env (List.append l boundvarlist) t')
      
  in

  match formulalist with
  | []-> (None,status)
  | _ -> 
    let formula = transformTermBase EmptyEnv [] "and" formulalist in
    let fformula = if satprov then formula else I.sigsymb "not" [formula] in
    (Some fformula,status)
