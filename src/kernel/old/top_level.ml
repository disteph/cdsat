(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
  
(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

       
let init (type a)
      (plDS : ((module Theories.Prop.APIplugin.PlugDSType),a) General.Opt.t)
      th =
  (* fun parsed  -> *)

  let open Theories.Register in
  let theories =
    match !Flags.addtheories, !Flags.notheories, th with
    | Some l,Some l', Some parsed ->
       HandlersMap.diff (HandlersMap.union (get parsed) (get l)) (get l')
    | None,Some l',_
      | _,Some l',None   -> HandlersMap.diff all_theories (get l')
    | None,None,None     -> all_theories
    | Some l, None,_  
      | None,None,Some l -> get l
  in
  print_endline(Dump.toString (fun p->
                    p "Using theories: %a" HandlersMap.pp theories));

  let (module WBThMod) = Combo.make theories (module PlDS)
  in
  
  let formula =
    Theories.Prop.ForParsing.toForm 
      (if !Flags.mode
       then Theories.Prop.ForParsing.bC Top.Symbols.IsTrue [parsed]
       else parsed)
  in
  Dump.print ["top_level",1]
    (fun p->
      p "I am now starting: %t"
        (if !Flags.printrhs
         then fun fmt -> Theories.Prop.Formulae.FormulaB.pp fmt formula
         else fun fmt -> ()));
  dealWanswer(WBThMod.PropModule.machine formula)


(* Now we run on a particular input, trying out various parsers *)

let parseNrun myPluginG get_plugin input =
  let rec trying = function
    | (module MyParser:Top.Parser.ParserType)::other_parsers ->
      begin
	try 
          let aft = MyParser.glance input in
          let prop4parsing = (module Theories.Prop.ForParsing : Top.Specs.ForParsing with type t = Theories.Prop.ForParsing.t) in
          let pair = match MyParser.parse aft (Typing.forParser prop4parsing) with
            | Some parsable, b -> Some(parsable Top.Sorts.Prop),b
            | None, b -> None, b
          in
          print_endline(Dump.toString (fun p->p "Successfully parsed by %s parser." MyParser.name));
	  init myPluginG get_plugin (MyParser.guessThDecProc aft) pair
	with Top.Parser.ParsingError s | Typing.TypingError s ->
          print_endline(Dump.toString (fun p->p "Parser %s could not parse input, because \n%s" MyParser.name s));
          trying other_parsers
      end
    | [] -> print_endline "No parser seems to work for this input."; function _ -> None
  in
  let parselist = match !Flags.parser with
    | None   -> Parsers.Register.all_parsers
    | Some l -> List.fold (fun s l -> (Parsers.Register.get s)::l) l []
  in
  trying parselist
