(* Tests *)

open Kernel
open Plugin
open Search
open Printf

let write_to_file filename s =
  let chan = open_out filename in 
    fprintf chan "%s\n" s;  
    close_out chan

module Tests (MyTheory:Theory.Type)(P:Plugin.Type with type literals = MyTheory.Atom.t) = struct

  open P
  module Src   = ProofSearch(MyTheory)(UF)(UFSet)(UASet)
  module Strat = Strategy(Src.FE)

  let orig_seq my_formula = Src.FE.Seq.EntUF(UASet.empty,UFSet.add my_formula UFSet.empty, UFSet.empty, UFSet.empty,Src.FE.emptypolmap)
    
  let go formula = print_endline("---");
    if !Flags.debug>0 then print_endline("I am now starting: "^if !Flags.printrhs then Src.FE.Form.toString formula else "");
    Strat.solve(Src.machine (orig_seq formula) Strat.initial_data)
      
  module MyParser = MyTheory.Parser(UF)

  let treatfile filename = print_endline("===========================");
    print_endline("Treating file "^filename);
    go(MyParser.parse filename)

  let treatdir a = print_endline("Treating directory "^a);
    let b = Sys.readdir(a) in
      Array.sort Pervasives.compare b;
      for i=0 to Array.length b-1 do
	let _ = treatfile (a^Filename.dir_sep^b.(i)) in ();
      done
end
