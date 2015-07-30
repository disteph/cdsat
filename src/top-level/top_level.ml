(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
open Kernel

(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

let init th =
  let theories =
    match !Flags.notheories,th with
    | Some l,_    -> Theories_register.get_no l
    | None,Some l -> Theories_register.get l
    | None,None   -> Theories_register.get_no []
  in

  let mypluginG = PluginsG_register.get !Flags.mypluginG in
  let module MyPluginG = (val mypluginG) in

  let myplugin = Plugins_register.get !Flags.myplugin theories in
  let module MyPlugin = (val myplugin) in

  let module PS = Prop.Search.ProofSearch(MyPluginG.DS) in
  let propds = (module PS.Semantic: Top.Specs.Semantic with type t = PS.Semantic.t) in

  let mode : (module Prop.Interfaces_theory.DecProc with type DS.formulae = PS.Semantic.t)
      = if !Flags.mode
        then
          let datalistWprop = Combo.ConsData(propds,MyPlugin.dataList) in
          let wb,Combo.ConsProj(asForm,projlist) = Combo.make theories datalistWprop in
          let module WB = (val wb) in
          let module Res = ForGround.GTh2Th(WB)
                (struct type formulae = PS.Semantic.t let asF = asForm end)
                (MyPlugin.Strategy(struct include WB let projList = projlist end))
          in
          (module Res)
        else (* FirstOrder.make propds; *)
          failwith "First-Order not working at the minute, please try again in 6 months"
  in
  let module Mode  = (val mode) in

  let module Src   = PS.Make(Mode) in
  let module Strat = MyPluginG.Strategy(Src.FE) in
  fun f stringOrunit ->
    let result = match f with 
      | None,_                                   -> print_endline("No formula to treat");None 
      | Some _,None       when !Flags.skipunknown-> print_endline("Skipping problem with no expectation");None
      | Some _,Some(true) when !Flags.skipunsat  -> print_endline("Skipping problem expected to be UNSAT/provable");None
      | Some _,Some(false)when !Flags.skipsat    -> print_endline("Skipping problem expected to be SAT/unprovable");None
      | Some parsed,c    ->
	try 
          let formula = Prop.ForParsing.toForm parsed in
	  let d =
	    Dump.msg
              (Some(fun p->p "I am now starting: %t"
                (if !Flags.printrhs then fun fmt -> Prop.Formulae.FormulaB.print_in_fmt fmt formula else fun fmt -> ())))
              None None;
	    Strat.solve(Src.machine formula Strat.initial_data)
	  in 
	  print_endline(match c,d with
	  |None ,_                         -> "Nothing expected"
	  |Some true, Src.FE.Provable _    -> "Expected Provable (UNSAT), got it"
	  |Some true, Src.FE.NotProvable _ -> "*** WARNING ***: Expected Provable (UNSAT), got Unprovable (SAT)"
	  |Some false,Src.FE.Provable _    -> "*** WARNING ***: Expected Unprovable (SAT), got Provable (UNSAT)"
	  |Some false,Src.FE.NotProvable _ -> "Expected Unprovable (SAT), got it"
	  );
	  Some d
        with PluginG.PluginAbort s -> Dump.Kernel.fromPlugin(); Dump.Kernel.report s; None
    in 
    match result with
    | None -> None
    | Some r when stringOrunit-> Some(A(Dump.toString (fun p->p "%a" Src.FE.print_in_fmt r)))
    | Some r                  -> Some(F())


(* Now we run on a particular input, trying out various parsers *)

let parseNrun input =
  let rec trying = function
    | i when i < Array.length Parsers_register.bank ->
      let module MyParser = (val Parsers_register.bank.(i):Top.Parser.ParserType) in
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
          trying (i+1)
      end
    | _ -> print_endline "No parser seems to work for this input."; function _ -> None
  in
  trying 0


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
	       aux^"Trying to prove: "^text^"\n\n"^(match output true with Some(A o) -> o | _ -> "No run")^"\n\\vspace{30pt}\n\n") ;
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



(* treatexamples treats the examples provided by the theory *)

(* let treatexamples pack () = *)
(*   let mythplug = guessThPlug("") in *)
(*   let module MyThPlug    = (val mythplug:ThPlug) in *)
(*   let module R           = Run(MyThPlug) in *)
(*   let module MyStructure = MyThPlug.MyTheory.ForParsing(R.FE.Form) in *)
(*   let examples = MyStructure.examples: ((unit->F.t)*bool) list *)
(*   in *)
(*   let rec aux acc = function *)
(*     | []            -> pack.final acc *)
(*     | (a,expect)::l ->  *)
(* 	let formulastring = Dump.stringOf R.FE.Form.print_in_fmt (a()) in *)
(*         print_endline ("---\n"^formulastring); *)
(*         let newacc = pack.accu acc ("$"^formulastring^"$") (R.go(Some a,Some expect)) in *)
(*         aux newacc l *)
(*   in *)
(*   aux pack.init examples *)

let treatprimitives () =
  if !Flags.latex
  then (treatname latex_wrap, (* treatexamples latex_wrap, *) treatstdin latex_wrap)
  else (treatname empty_wrap, (* treatexamples empty_wrap, *) treatstdin empty_wrap)
