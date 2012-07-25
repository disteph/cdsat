(* Tests *)

open Sequents;;
open Strategy;;
open Search;;
open Io;;

module Tests =
  functor (US:User) ->
    (struct

       include US
       module FE = FrontEnd(UF)(UFSet)(UASet)
       module Strat = Strategy(FE)
       module Src = ProofSearch(UF)(UFSet)(UASet)
       module Gen = Generate(UF)

       let orig_seq my_formula = FE.Seq.EntUF(UASet.empty,UFSet.add my_formula UFSet.empty, UFSet.empty, UFSet.empty, UFSet.empty,Sequents.Pol.empty);;
       let go formula = 
	 print_endline("===");
	 print_endline("I am now starting: "^(Src.Form.toString formula));
	 Strat.solve(Src.machine (orig_seq formula));;

       (*
	 let treatfile filename = print_list  (list_from_string (read_from_file(filename)) [] 0);;
       *)

       (* let treatfile filename =  print_string ((read_from_file(filename)));; *)
       (* let treatfile filename = print_endline(printformula (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0)))));; *)
       let treatfile filename = go (Gen.generate_cnf(Io.parse_cnf_file (Io.list_from_string (Io.read_from_file(filename)) [] 0)));;

       let treatdir a = 
	 let b = Sys.readdir(a) in
	   for i=0 to Array.length b-1 do
	     let _ = treatfile (a^Filename.dir_sep^b.(i)) in ();
	   done
       ;;


     end)
;;
