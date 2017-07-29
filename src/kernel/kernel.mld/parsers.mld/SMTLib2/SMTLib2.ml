(* SMTLib2 parser *)

open SMTLib2_tools
open Smtlib2_ast
open Parser
open Parse
       
type 'a environment =
  | EmptyEnv
  | ConsEnv of 'a*('a environment)*('a environment)

let rec search symb = function
  | EmptyEnv                    -> None
  | ConsEnv((s,t),e,l)
       when [%eq:string] s symb -> Some(t,e)
  | ConsEnv(_,_,l)              -> search symb l

let getconst = function
  | SpecConstsDec   (_,s)
  | SpecConstNum    (_,s)
  | SpecConstString (_,s)
  | SpecConstsHex   (_,s)
  | SpecConstsBinary(_,s) -> s

let getsymbSymbol = function
  | Symbol(_,symb)
  | SymbolWithOr(_,symb)-> symb

let rec getsort = function
  | SortIdentifier(_,IdSymbol(_,symb))
    -> Sort(getsymbSymbol symb,[])
  | SortIdSortMulti(_,IdSymbol(_,symb),(_,l)) 
    -> Sort(getsymbSymbol symb,List.map getsort l)
  | _ -> raise (ParsingError "Could not parse sort")

let getsortedvar (SortedVarSymSort(_,symb,sort)) =
  getsymbSymbol symb, getsort sort

type 'a getsymbType =
| AppliedSymbol of string*(term list)*('a environment)
| Quantif of bool*((string*sort) list)*term

let rec getsymb env = function
  | TermLetTerm(_,(_,l),t) -> 
      let newenv = 
	List.fold
	  (fun (VarBindingSymTerm(_,sym,t)) locenv -> ConsEnv((getsymbSymbol sym,t),env,locenv))
	  l env
	  in
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

type afterglance = {
  theory: string option;            (* Theory, if specified *)
  satprov: bool;                    (* Whether we prove (true) or refute (false) *)
  status: bool option;              (* The status of the problem (Some true) if provable/unsat, (Some false) if counter-model/sat *)
  decso: unit SM.t;                 (* Declared sorts: set of strings *)
  decsym: (sort * (sort list)) SM.t;  (* Declared symbols: strings mapped to their arity:
                                    a pair whose first component is the output sort, second component is the list of arguments' sorts *)
  form_list : Smtlib2_ast.term list (* The problem instance *)
}

let transformCommand command aft = match command with
  | CAssert(_,t)                 -> {aft with form_list = t::aft.form_list}
  | CSetLogic(_,symbol)          ->
     begin
       match aft.theory with
       | Some prevtheory -> 
          begin
            print_endline("Trying to declare theory twice: was "^prevtheory
                          ^", now declared as "^getsymbSymbol symbol^". Keeping "^prevtheory);
            aft
          end
       | None   -> {aft with theory = Some(getsymbSymbol symbol)}
     end
  | CSetInfo(_,AttributeKeywordValue(_,":status",attribute)) ->
     begin
       match aft.status with
       | Some prevstatus -> 
          begin
            print_endline("Trying to declare status twice: was "^string_of_stat aft.status
                          ^", now declared as "^string_of_stat (getStatus attribute)^". Keeping "^string_of_stat aft.status);
            aft
          end
       | None  -> aft
     end
  | CCheckSat _                      -> {aft with satprov = false}
  | CDeclareSort(_,symbol,arity)     ->
     if int_of_string arity != 0
     then raise (ParsingError ("Trying to declare sort "^getsymbSymbol symbol^" with arity "^arity
                               ^", but Psyche can only handle sorts of arity 0"))
     else {aft with decso = SM.add (getsymbSymbol symbol) () aft.decso}
  | CDeclareFun(_,symbol,(_,l),sort) ->
     {aft with decsym = SM.add (getsymbSymbol symbol) (getsort sort,List.map getsort l) aft.decsym}
  | _                                -> aft

let transformCommands (Commands(_,(_,li))) =
  List.fold transformCommand 
    li
    {
      theory = None;
      satprov = true;
      status = None;
      decso = SM.empty;
      decsym = SM.empty;
      form_list = []
    }

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
	  
let guessThDecProc aft =
  match aft.theory with
    | None   -> None
    | Some "QF_BOOL"-> Some ["bool"]
    | Some "QF_UF"  -> Some ["CC"; "bool"; "IfThenElse"]
    | Some "QF_LRA" -> Some ["LRA"; "CC"; "bool"; "IfThenElse"]
    | Some "QF_LIA" -> Some ["LIA"; "CC"; "bool"; "IfThenElse"]
    | Some "QF_AX"  -> Some ["Arrays"; "CC"; "bool"; "IfThenElse"]
    | Some a -> Some [a; "CC"; "bool"; "IfThenElse"]

let rec list_index a accu = function
  | [] -> None
  | (b,so)::l when [%eq:string] a b -> Some (accu,so)
  | _::l -> list_index a (accu+1) l

let parse (type t) aft interpreter =

  let (module I: InterpretType with type t=t) =
    interpreter ~decsorts:(SM.fold (fun s () l -> s::l) aft.decso [])
  in
  
  let rec transformTermBase env boundvarlist s l =
    match list_index s 0 boundvarlist,l with
    | Some(db,so),[]  -> I.boundsymb db so
    | Some(_,_) , _   -> raise (ParsingError "Higher-order quantification")
    | None,_          ->
      let l' = List.map (transformTerm env boundvarlist) l in
      if SM.mem s aft.decsym
      then I.decsymb s (SM.find s aft.decsym) l'
      else I.sigsymb s l'
      
  and transformTerm env boundvarlist t =
    match getsymb env t with
    | AppliedSymbol(s,l,newenv) -> transformTermBase newenv boundvarlist s l
    | Quantif(b,l,t') -> I.quantif b (List.map snd l) (transformTerm env (List.append l boundvarlist) t')
      
  in

  let aux t =
    let formula = transformTerm EmptyEnv [] t
    in
    if not aft.satprov then formula else I.sigsymb "not" [formula]
  in
  List.map aux aft.form_list, aft.status
