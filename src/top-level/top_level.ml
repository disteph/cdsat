(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open General.Sums
open Kernel.Interfaces_theory

(* Module type for Theory+DecProc+Plugin compatible with each other *)

module type ThPlug = sig
  module MyTheory : ThManager.Type
  module MyPlugin : Plugin.Type with type DS.UASet.e = MyTheory.DS.IAtom.t
                                and  type DS.UF.lit  = MyTheory.DS.Atom.t
end

(* Variables containing the Theory+DecProc (resp. generic plugin)
forced by the user (from command-line) *)

let mytheory  = ref None 
let mygplugin = ref None

(* guessThPlug guesses the pair Theory+DecProc + Plugin, taking into
account user input, and argument s, which is a theory name (string),
as possibly indicated by the parser when glancing at the input *)

let guessThPlug s =
  let myth = 
    match !mytheory with
      | Some a -> a
      | None ->
	  try Theories_register.getbyname s
	  with Not_found 
	    -> (Dump.msg (Some(fun p->p "Could not find theory %s in the theory signatures register, using Empty(Propositional)" s)) None None;
		Theories_register.bank.(0))
  in
  let module MyDecProc  = (val myth:Theory.Type) in
  let module MyTheory   = ThManager.Make(MyDecProc) in
  let plugin =
    match !mygplugin with
    | Some a -> a
    | None   -> 
      try 
        match MyTheory.sugPlugin with
        | Some a -> Gplugins_register.getbyname a
        | None -> raise (Gplugins_register.NotFound "Theory did not suggest any plugin")
      with Gplugins_register.NotFound msg
        -> (Dump.msg (Some(fun p->p "Could not find plugin %s in the gplugins register, DPLL_WL" msg)) None None;
	    Gplugins_register.bank.(1))
  in
  let module MyGPlugin = (val plugin) in
  let module MyThPlug  = struct
    module MyTheory = MyTheory 
    module MyPlugin = MyGPlugin(MyTheory.DS)
  end
  in (module MyThPlug:ThPlug)


(* Given a compatibleTheory+DecProc + Plugin, module Run provides the
main run function: go *)

module Run(MyThPlug:ThPlug)= struct

  open MyThPlug
  
  module Src   = Kernel.Search.ProofSearch(MyTheory)(MyPlugin.DS)
  module FE    = Src.FE
  module Strat = MyPlugin.Strategy(FE)

(* Constraint.topconstraint *)

  let go f stringOrunit =
    let result = match f with 
      | None,_                                   -> print_endline("No formula to treat");None 
      | Some _,None       when !Flags.skipunknown-> print_endline("Skipping problem with no expectation");None
      | Some _,Some(true) when !Flags.skipunsat  -> print_endline("Skipping problem expected to be UNSAT/provable");None
      | Some _,Some(false)when !Flags.skipsat    -> print_endline("Skipping problem expected to be SAT/unprovable");None
      | Some formula,c    ->
	try 
	  let d =
	    Dump.msg (Some(fun p->p "I am now starting: %t" (if !Flags.printrhs then fun fmt -> FE.Form.print_in_fmt fmt (formula()) else fun fmt -> ()))) None None;
	    Strat.solve(Src.machine (formula()) Strat.initial_data)
	  in 
	  print_endline(match c,d with
	  |None ,_                 -> "Nothing expected"
	  |Some true, FE.Provable _ -> "Expected Provable (UNSAT), got it"
	  |Some true, FE.NotProvable _    -> "*** WARNING ***: Expected Provable (UNSAT), got Unprovable (SAT)"
	  |Some false,FE.Provable _ -> "*** WARNING ***: Expected Unprovable (SAT), got Provable (UNSAT)"
	  |Some false,FE.NotProvable _    -> "Expected Unprovable (SAT), got it"
	  );
	  Some d
        with Plugin.PluginAbort s -> Dump.Kernel.fromPlugin(); Dump.Kernel.report s; None
    in 
    match result with
    | None -> None
    | Some r when stringOrunit-> Some(A(Dump.toString (fun p->p "%a" FE.print_in_fmt r)))
    | Some r                  -> Some(F())

end



(* Now we run on a particular input, trying out various parsers *)

let parseNrun input =
  let rec trying = function
    | i when i < Array.length Parsers_register.bank ->
      let module MyParser = (val Parsers_register.bank.(i):Parser.Type) in
      begin
	try 
	  let aft                = MyParser.glance input in
	  let module MyThPlug    = (val guessThPlug(MyParser.guessThDecProc aft):ThPlug) in
	  let module R           = Run(MyThPlug) in
	  let module MyTheory    = MyThPlug.MyTheory in
	  let module MyForParse  = MyTheory.ForParsing(R.FE.Form) in
          let open TheoryParsing in
	  let inter = (module ForParser(MyForParse):InterpretType with type t = Kernel.Sorts.t -> MyForParse.t) in
	  let (a,b) = match MyParser.parse MyTheory.names aft inter with
	    | None,b  -> None,b
	    | Some a,b-> Some(fun ()-> MyForParse.toForm (a Kernel.Sorts.Prop)),b
	  in
          Dump.msg (Some (fun p->p "Successfully parsed by %s parser." MyParser.name)) None None;
	  R.go(a,b)
	with Parser.ParsingError s | TheoryParsing.TypingError s ->
          if !Flags.debug>0 
          then Dump.msg (Some (fun p->p "Parser %s could not parse input, because \n%s" MyParser.name s)) None None;
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

let treatexamples pack () =
  let mythplug = guessThPlug("") in
  let module MyThPlug    = (val mythplug:ThPlug) in
  let module R           = Run(MyThPlug) in
  let module MyStructure = MyThPlug.MyTheory.ForParsing(R.FE.Form) in
  let examples = MyStructure.examples in
  let rec aux acc = function
    | []            -> pack.final acc
    | (a,expect)::l -> 
	let formulastring = Dump.stringOf R.FE.Form.print_in_fmt (a()) in
        print_endline ("---\n"^formulastring);
        let newacc = pack.accu acc ("$"^formulastring^"$") (R.go(Some a,Some expect)) in
        aux newacc l
  in
  aux pack.init examples

let treatprimitives () =
  if !Flags.latex
  then (treatname latex_wrap, treatexamples latex_wrap, treatstdin latex_wrap)
  else (treatname empty_wrap, treatexamples empty_wrap, treatstdin empty_wrap)
