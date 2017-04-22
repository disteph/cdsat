(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
  
(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

       
let init (type uaset)(type uf)(type ufset)
      (plDS : (module Theories.Prop.APIplugin.PlugDSType with type UASet.t=uaset
                                                          and type UF.t = uf
                                                          and type UFSet.t=ufset))
      (module MyParser:Parsers.Parser.Type)
      input =

  let aft = MyParser.glance input in

  (* Now we parse *)
  let i = (module Parsers.Typing.ForParsing : Top.Specs.ForParsing with type t = Top.Terms.TermB.t) in
  let parsed, expected =
    match MyParser.parse aft (Parsers.Typing.forParser i) with
    | Some parsable, b -> parsable Top.Sorts.Prop,b
    | None, b -> Parsers.Typing.ForParsing.bC Top.Symbols.True [], b
  in
  let termB =
    if !Flags.mode
    then Parsers.Typing.ForParsing.bC Top.Symbols.IsTrue [parsed]
    else parsed
  in
  print_endline("We want to prove: "^Top.Terms.TermB.show termB);

  (* Now we look at the theories involved *)
  let th  = MyParser.guessThDecProc aft in
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

  (* Now that we know the theories, we build the combined datastructures *)
  
  Combo.make termB expected theories plDS

