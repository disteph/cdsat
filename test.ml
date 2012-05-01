(* Tests *)

open Zipper;;
open Sequents;;
open Strategy;;
open Search;;
open Io;;

module Tests =
  functor (US:UserStrategy) ->
    (struct

       include US
       module Src = ProofSearch(US)
       module Gen = Generate(F)
       include Gen

       let orig_seq my_formula = Src.Seq.EntUF(ASet.empty,FSet.add my_formula FSet.empty, FSet.empty, FSet.empty, FSet.empty);;
       let orig_situation my_seq = fun () -> Src.lk_solve (fun x->x) false my_seq;;
       let go formula = 
	 print_endline("===");
	 failcount := 0;
	 tiredcount := 0;
	 successcount := 0;
	 let h = orig_seq formula in 
	   Src.solve (add_left (orig_situation h,h) empty_zip);;

       (*
	 let treatfile filename = print_list  (list_from_string (read_from_file(filename)) [] 0);;
       *)

       (* let treatfile filename =  print_string ((read_from_file(filename)));; *)
       (* let treatfile filename = print_endline(printformula (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0)))));; *)
       let treatfile filename = go (generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0)));;

       let treatdir a = 
	 let b = Sys.readdir(a) in
	   for i=0 to Array.length b-1 do
	     treatfile (a^Filename.dir_sep^b.(i));
	   done
       ;;

     end)
;;
