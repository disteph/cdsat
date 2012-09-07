(* Tests *)

open Kernel
open Plugins

open Plugin
open Search
open Io

module Tests (P:Plugin.Type) = struct

  include P
  module Src   = ProofSearch(UF)(UFSet)(UASet)
  module Strat = Strategy(Src.FE)

  let orig_seq my_formula = Src.FE.Seq.EntUF(UASet.empty,UFSet.add my_formula UFSet.empty, UFSet.empty, UFSet.empty,Sequents.Pol.empty)
    
  let go formula = print_endline("---");
    if !Flags.debug>0 then print_endline("I am now starting: "^if !Flags.printrhs then Src.FE.Form.toString formula else "");
    Strat.solve(Src.machine (orig_seq formula) Strat.initial_data)
      
  module Gen = Generate(UF)

  let treatfile filename = print_endline("===========================");
    print_endline("Treating file "^filename);
    go (Gen.generate_cnf(Io.parse_cnf_file (Io.list_from_string (Io.read_from_file(filename)) [] 0)))

  let treatdir a = print_endline("Treating directory "^a);
    let b = Sys.readdir(a) in
      Array.sort Pervasives.compare b;
      for i=0 to Array.length b-1 do
	let _ = treatfile (a^Filename.dir_sep^b.(i)) in ();
      done
end
