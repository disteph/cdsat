(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
open Kernel
  
(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

let init th =

  let (module MyPluginG) = PluginsG_register.get !Flags.mypluginG in

  let module PS = Prop.Search.ProofSearch(MyPluginG.DS) in
  let propds = (module PS.Semantic: Top.Specs.DataType with type t = PS.Semantic.t) in

  let (module Mode) : (module Prop.Interfaces_theory.DecProc with type DS.formulae = PS.Semantic.t)
    = if !Flags.mode
      then
        let open Theories_register in
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
                          p "Using theories: %a" HandlersMap.print_in_fmt theories));
        let (module MyPlugin) = Plugins_register.get !Flags.myplugin theories in
        let datalistWprop = Combo.ConsData(propds,MyPlugin.dataList) in
        let (module WB), Combo.ConsProj(asForm,projlist) = Combo.make theories datalistWprop in
        let module Res = ForGround.GTh2Th(WB)
                           (struct type formulae = PS.Semantic.t let asF = asForm end)
                           (MyPlugin.Strategy(struct include WB let projList = projlist end))
        in
        (module Res)
      else
        let module FO = FirstOrder.MyTheory.Make(PS.Semantic) in
        (module FO)
  in

  let module Src   = PS.Make(Mode) in
  let module Strat = MyPluginG.Strategy(Src.FE) in
  fun f stringOrunit ->
    let result = match f with 
      | None,_                                   -> print_endline("No formula to treat");None 
      | Some _,None       when !Flags.skipunknown-> print_endline("Skipping problem with no expectation");None
      | Some _,Some(true) when !Flags.skipunsat  -> print_endline("Skipping problem expected to be UNSAT/provable");None
      | Some _,Some(false)when !Flags.skipsat    -> print_endline("Skipping problem expected to be SAT/unprovable");None
      | Some parsed, expected ->
	try 
          let formula =
            Prop.ForParsing.toForm 
              (if !Flags.mode
               then Prop.ForParsing.bC Top.Symbols.IsTrue [parsed]
               else parsed)
          in
	  let answer =
	    Dump.print ["top_level",1]
              (fun p->
                p "I am now starting: %t"
                  (if !Flags.printrhs
                   then fun fmt -> Prop.Formulae.FormulaB.print_in_fmt fmt formula
                   else fun fmt -> ()));
	    Strat.solve(Src.machine formula Strat.initial_data)
	  in 
	  print_endline(match expected, answer with
	  |None ,_                         -> "Nothing expected"
	  |Some true, Src.FE.Provable _    -> "Expected Provable (UNSAT), got it"
	  |Some true, Src.FE.NotProvable _ -> "*** WARNING ***: Expected Provable (UNSAT), got Unprovable (SAT)"
	  |Some false,Src.FE.Provable _    -> "*** WARNING ***: Expected Unprovable (SAT), got Provable (UNSAT)"
	  |Some false,Src.FE.NotProvable _ -> "Expected Unprovable (SAT), got it"
	  );
	  Some answer
        with PluginG.PluginAbort s -> Dump.Kernel.fromPlugin(); Dump.Kernel.report s; None
    in 
    match result with
    | None -> None
    | Some r when stringOrunit ->
       if !Flags.latex then
         let display = !Dump.display in
         Dump.display := Dump.Latex;
         let ans = Some(Case1(Dump.toString (fun p->p "%a" Src.FE.print_in_fmt r)))
         in Dump.display := display; ans
       else Some(Case1(Dump.toString (fun p->p "%a" Src.FE.print_in_fmt r)))
    | Some _ -> Some(Case2())


(* Now we run on a particular input, trying out various parsers *)

let parseNrun input =
  let rec trying = function
    | (module MyParser:Top.Parser.ParserType)::other_parsers ->
      begin
	try 
          let aft = MyParser.glance input in
          let prop4parsing = (module Prop.ForParsing : Top.Specs.ForParsing with type t = Prop.ForParsing.t) in
          let pair = match MyParser.parse aft (Typing.forParser prop4parsing) with
            | Some parsable, b -> Some(parsable Top.Sorts.Prop),b
            | None, b -> None, b
          in
          print_endline(Dump.toString (fun p->p "Successfully parsed by %s parser." MyParser.name));
	  init (MyParser.guessThDecProc aft) pair
	with Top.Parser.ParsingError s | Typing.TypingError s ->
          print_endline(Dump.toString (fun p->p "Parser %s could not parse input, because \n%s" MyParser.name s));
          trying other_parsers
      end
    | [] -> print_endline "No parser seems to work for this input."; function _ -> None
  in
  let parselist = match !Flags.parser with
    | None   -> Parsers_register.all_parsers
    | Some l -> List.fold_left (fun l s -> (Parsers_register.get s)::l) [] l
  in
  trying parselist


(* Inhabitant of type ('a,'b)wrap describe how to wrap a series of Psyche runs:
- init is the initial data before any run is made
- accu is what do do after every run ('b option is the return type of the run) 
- final is what to do with the accumulated data
Think of init and accu as what will be fed, together with a list of
inputs, to a List.fold_left call. And final as what will be done to
the result of that call. *)

type ('a,'b)wrap = {init: 'a ; accu: 'a->string->(bool->'b option)->'a ; final: 'a->unit}

let latex_wrap =
  { init  = "";
    accu  = (fun aux text output ->
	       aux^"Trying to prove: "^text^"\n\n"^(match output true with Some(Case1 o) -> o | _ -> "No run")^"\n\\vspace{30pt}\n\n") ;
    final = IO.write_to_file "latex/output.tex" }

let empty_wrap =
  { init  = ();
    accu  = (fun _ _ output -> let _ = output false in ()) ;
    final = fun () -> () }

(* treatstdin uses stdin as input *)

let treatstdin pack () =
  print_endline("===========================");
  print_endline("Treating stdin");
  pack.final(pack.accu pack.init "Treating stdin" (parseNrun (IO.read_from_stdin())))

(* treatfile_aux uses file called filename as input *)

let treatfile_aux accu aux filename = 
  let filename4latex  = "file \\verb="^filename^"=" in
  let filename4stdout = "===========================\nTreating file "^filename in
    print_endline filename4stdout;
    accu aux filename4latex (parseNrun(IO.read_from_file filename))

(* treatfile wraps treatfile_aux in case 1 file is treated *)

let treatfile pack filename =
  pack.final(treatfile_aux pack.accu pack.init filename)

(* treatdir wraps treatfile_aux in case a whole directory is treated *)

let collect_sort s =
  match Sys.os_type with
    | "Unix" when !Flags.sizesort ->
	let open Unix in
	let size_of s = (stat s).st_size in
	let l = List.map (fun filename -> (filename,size_of filename)) s in
	let l'= List.sort (fun (a,b)(c,d)->Pervasives.compare b d) l in 
	  List.map (fun (filename,size) -> filename) l'
    | _ -> List.sort Pervasives.compare s

let treatdir pack dirname =
  print_endline("Treating directory "^dirname);
  let b = collect_sort(List.map 
			 (fun filename -> dirname^Filename.dir_sep^filename) 
			 (Array.to_list (Sys.readdir dirname))) in
  let rec aux acc = function
    | []          -> pack.final acc 
    | name::l ->
      let newacc = if not(Sys.is_directory name)
	then treatfile_aux pack.accu acc name
	else acc
      in aux newacc l
  in
  aux pack.init b
    
(* treatname does both *)

let treatname pack name =
  if Sys.is_directory name
  then treatdir pack name
  else treatfile pack name

let treatprimitives () =
  if !Flags.latex
  then (treatname latex_wrap, treatstdin latex_wrap)
  else (treatname empty_wrap, treatstdin empty_wrap)
