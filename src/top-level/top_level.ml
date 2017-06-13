(************************************)
(* Main entry point for Psyche runs *)
(************************************)

type _ stringOrunit =
  | String : string stringOrunit
  | Unit   : unit stringOrunit
       
let run parser input =
  let (module Pl)  = Plugins.Register.get !PFlags.myplugin in
  let (module PlG) = PluginsG.Register.get !PFlags.mypluginG in
  let (module K) = Kernel.Top_level.init
                     (module PlG.DS)
                     ~parser
                     input
  in
  let result = match K.expected with 
    | None        when !PFlags.skipunknown-> print_endline("Skipping problem with no expectation");None
    | Some(true)  when !PFlags.skipunsat  -> print_endline("Skipping problem expected to be UNSAT/provable");None
    | Some(false) when !PFlags.skipsat    -> print_endline("Skipping problem expected to be SAT/unprovable");None
    | _ ->
       try 
         let module P = Pl.Make(struct
                            include K
                            include PlG.Strategy(K.PropModule.FE)
                            include Plugins.LoadPluginsTh.Make(K)
                          end)
         in
         let answer = P.solve() in
         P.clear();
	 print_endline(
             match K.expected, answer with
	     |None ,_              -> "Nothing expected"
	     |Some true, K.UNSAT _ -> "Expected Provable (UNSAT), got it"
	     |Some true, K.SAT _   -> "*** WARNING ***: Expected Provable (UNSAT), got Unprovable (SAT)"
	     |Some false,K.UNSAT _ -> "*** WARNING ***: Expected Unprovable (SAT), got Provable (UNSAT)"
	     |Some false,K.SAT _   -> "Expected Unprovable (SAT), got it"
	     |Some _,K.NotAnsweringProblem -> "You did not answer the right problem"
	   );
	 Some answer
       with PluginsG.PluginG.PluginAbort s
            -> Dump.Kernel.fromPlugin();
               Dump.Kernel.report s;
               None
  in 

  let aux : type s. s stringOrunit -> s option =
  fun stringOrunit ->
  match result,stringOrunit with
  | None, _        -> None
  | Some r, Unit   -> Some()
  | Some r, String ->
     let display = !Dump.display in
     if !Flags.latex then
       Dump.display := Dump.Latex;
     let ans =
       match r with
       | K.UNSAT assign -> Some(Dump.toString (fun p->p "%a" K.WB.pp assign))
       | K.SAT assign   -> Some(Dump.toString (fun p->p "%a" K.WB.pp assign))
       | K.NotAnsweringProblem -> None
     in
     Dump.display := display;
     ans
                 in aux


let parseNrun input =
  let parsers = match !PFlags.parser with
    | Some parsers -> List.map Kernel.Parsers.Register.parse parsers
    | None -> Kernel.Parsers.Register.all
  in
  let rec trying = function
    | parser::other_parsers ->
      begin
	try 
          run parser input
	with Kernel.Parsers.Parser.ParsingError s
           | Kernel.Parsers.Typing.TypingError s ->
              print_endline(Dump.toString (fun p->p "Parser %a could not parse input, because \n%s" Kernel.Parsers.Register.pp parser s));
              trying other_parsers
      end
    | [] -> print_endline "No parser seems to work for this input."; function _ -> None
  in
  trying parsers

                           
(* Inhabitant of type ('a,'b)wrap describe how to wrap a series of Psyche runs:
- init is the initial data before any run is made
- accu is what do do after every run ('b option is the return type of the run) 
- final is what to do with the accumulated data
Think of init and accu as what will be fed, together with a list of
inputs, to a List.fold_left call. And final as what will be done to
the result of that call. *)

type ('a,'b)wrap = {init: 'a ;
                    accu: 'a->string->('b stringOrunit->'b option)->'a ;
                    final: 'a->unit}

let latex_wrap =
  { init  = "";
    accu  = (fun aux text output ->
      aux^"Trying to prove: "^text^"\n\n"
      ^(match output String with
        | Some s -> s
        | None -> "No run")^"\n\\vspace{30pt}\n\n") ;
    final = IO.write_to_file "latex/output.tex" }

let empty_wrap =
  { init  = ();
    accu  = (fun _ _ output -> let _ = output Unit in ()) ;
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
    | "Unix" when !PFlags.sizesort ->
	let open Unix in
	let size_of s = (stat s).st_size in
	let l = List.map (fun filename -> (filename,size_of filename)) s in
	let l'= List.sort (fun (a,b)(c,d)->[%ord:int] b d) l in 
	  List.map (fun (filename,size) -> filename) l'
    | _ -> List.sort [%ord:string] s

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
