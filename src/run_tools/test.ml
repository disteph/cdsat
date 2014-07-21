(************************************)
(* Main entry point for Psyche runs *)
(************************************)

open Lib.Sums

(* Module type for Theory+DecProc+Plugin compatible with each other *)

module type ThPlug = sig
  module MyThDecProc:Theories.ThDecProc
  module MyPlugin:Plugins.Type with type literals = MyThDecProc.Atom.t
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
	  try ThDecProc_register.getbyname s
	  with Not_found 
	      -> (if !Flags.debug>0
		  then print_endline("Could not find theory "^s^" in the theory signatures register, using Empty(Propositional)");
		  ThDecProc_register.bank.(0))
  in
  let module MyThDecProc = (val myth:Theories.ThDecProc) in
  let plugin =
    match !mygplugin,MyThDecProc.sugPlugin with
      | None,Some a -> a
      | _,_ -> 
	  let a = match !mygplugin with
	    | None   -> GPlugins_register.bank.(1)
	    | Some a -> a
	  in
	  let module MyGPlugin = (val a) in
	    (module MyGPlugin(MyThDecProc.Atom))
  in
  let module MyThPlug  = struct
    module MyThDecProc = MyThDecProc 
    module MyPlugin    = (val plugin)
  end
  in (module MyThPlug:ThPlug)


(* Given a compatibleTheory+DecProc + Plugin, module Run provides the
main run function: go *)

module Run(MyThPlug:ThPlug)= struct

  open MyThPlug.MyPlugin
  module Src   = Kernel.Search.ProofSearch(MyThPlug.MyThDecProc)(UF)(UFSet)(UASet)
  module FE    = Src.FE
  module Strat = Strategy(FE)

  let go f stringOrunit =
    let result = match f with 
      | None,_                                   -> print_endline("No formula to treat");None 
      | Some _,None       when !Flags.skipunknown-> print_endline("Skipping problem with no expectation");None
      | Some _,Some(true) when !Flags.skipunsat  -> print_endline("Skipping problem expected to be UNSAT/provable");None
      | Some _,Some(false)when !Flags.skipsat    -> print_endline("Skipping problem expected to be SAT/unprovable");None
      | Some b,c    ->
	let orig_seq = 
	  FE.Seq.EntUF(UASet.empty,UFSet.add (b()) UFSet.empty, UFSet.empty, UFSet.empty,FE.emptypolmap,MyThPlug.MyThDecProc.Constraint.init)
	in
	try 
	  let d =
	    if !Flags.debug>0 then print_endline("I am now starting: "^if !Flags.printrhs then FE.Form.toString (b()) else "");
	    Strat.solve(Src.machine orig_seq (Strat.initial_data orig_seq))
	  in 
	  print_endline(match c,d with
	  |None ,_                 -> "Nothing expected"
	  |Some true, FE.Success _ -> "Expected Success (UNSAT/provable), got it"
	  |Some true, FE.Fail _    -> "*** WARNING ***: Expected Success (UNSAT/provable), got Failure (SAT/unprovable)"
	  |Some false,FE.Success _ -> "*** WARNING ***: Expected Failure (SAT/unprovable), got Success (UNSAT/provable)"
	  |Some false,FE.Fail _    -> "Expected Failure (SAT/unprovable), got it"
	  );
	  Some d
        with Plugins.PluginAbort s -> if !Flags.debug>0 then print_endline s ; None
    in 
    match result with
    | None -> None
    | Some r when stringOrunit-> Some(A(FE.toString r))
    | Some r                  -> Some(F ())

end



(* Now we run on a particular input, trying out various parsers *)

let parseNrun input =
  let rec trying = function
    | i when i < Array.length Parsers_register.bank ->
      let module MyParser = (val Parsers_register.bank.(i):Parsers.Type) in
      begin
	try 
	  let aft      = MyParser.glance input in
	  let module MyThPlug    = (val guessThPlug(MyParser.guessThDecProc aft):ThPlug) in
	  let module R           = Run(MyThPlug) in
	  let module MyThDecProc = MyThPlug.MyThDecProc in
	  let module MyStructure = MyThDecProc.Structure(R.FE.Form) in
	  let inter = ThSig_tools.interpret MyThDecProc.Sig.forParsing MyStructure.st in
	  let (a,b) = match MyParser.parse MyThDecProc.Sig.forParser inter aft with
	    | None,b  -> None,b
	    | Some a,b-> Some(fun ()-> MyStructure.toform a),b
	  in
          Dump.msg (Some ("Successfully parsed by "^MyParser.name^" parser.")) None None;
	  R.go(a,b)
	with Parsers.ParsingError s | Theories.TypingError s ->
          if !Flags.debug>0 
          then Dump.msg (Some ("Parser "^MyParser.name^" could not parse input, because \n"^s)) None None;
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

type ('a,'b)wrap = {init: 'a ; accu: string->'a->(bool->'b option)->'a ; final: 'a->unit}

let latex_wrap =
  { init  = "";
    accu  = (fun text aux output ->
	       "Trying to prove: "^text^"\n\\vspace{10pt}\n\n"^(match output true with Some(A o) -> o | _ -> "No run")^"\\vspace{30pt}\n"^aux) ;
    final = IO.write_to_file "latex/output.tex" }

let empty_wrap =
  { init  = ();
    accu  = (fun text aux output -> let _ = output false in ()) ;
    final = fun () -> () }

(* treatstdin uses stdin as input *)

let treatstdin pack () =
  print_endline("===========================");
  print_endline("Treating stdin");
  pack.final(pack.accu "Treating stdin" pack.init (parseNrun (IO.read_from_stdin())))

(* treatfile_aux uses file called filename as input *)

let treatfile_aux accu filename aux = 
  let filename4latex  = "file \\verb="^filename^"=" in
  let filename4stdout = "===========================\nTreating file "^filename in
    print_endline filename4stdout;
    accu filename4latex aux (parseNrun(IO.read_from_file filename))

(* treatfile wraps treatfile_aux in case 1 file is treated *)

let treatfile pack filename =
  pack.final(treatfile_aux pack.accu filename pack.init)

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
  let rec aux = function
    | []          -> pack.init
    | name::l ->
	let next = aux l in
	  if not(Sys.is_directory name)
	  then treatfile_aux pack.accu name next
	  else next
  in
    pack.final(aux (List.rev b))

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
  let module MyStructure = MyThPlug.MyThDecProc.Structure(R.FE.Form) in
  let examples = MyStructure.examples in
  let rec aux = function
    | []            -> pack.init
    | (a,expect)::l -> 
	let formulastring = R.FE.Form.toString (a()) in
	  pack.accu ("$"^formulastring^"$") (aux l) (print_endline ("---\n"^formulastring); R.go(Some a,Some expect))
  in
    pack.final(aux examples)

let treatprimitives () =
  if !Flags.latex
  then (treatname latex_wrap, treatexamples latex_wrap, treatstdin latex_wrap)
  else (treatname empty_wrap, treatexamples empty_wrap, treatstdin empty_wrap)
