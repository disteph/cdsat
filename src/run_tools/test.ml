(* Tests *)

open Kernel
open Interfaces
open Plugin
open Search
open Printf

module Prepare(MyTheory:TheoryType)(P:Plugin.Type with type literals = MyTheory.Atom.t) = struct

  open P
  module Src   = ProofSearch(MyTheory)(UF)(UFSet)(UASet)
  module Strat = Strategy(Src.FE)

  let orig_seq my_formula = Src.FE.Seq.EntUF(UASet.empty,UFSet.add my_formula UFSet.empty, UFSet.empty, UFSet.empty,Src.FE.emptypolmap)
    
  let go formula = print_endline("---");
    if !Flags.debug>0 then print_endline("I am now starting: "^if !Flags.printrhs then Src.FE.Form.toString formula else "");
    Strat.solve(Src.machine (orig_seq formula) Strat.initial_data)

  module MyParser = MyTheory.Parser(UF)

  let print_test f = "Trying to prove: $"^Src.FE.Form.toString f^"$

  \\vspace{10pt}\n"^
    Src.FE.toString (go f)^"\\vspace{30pt}

  "

  let treatstdin () = print_endline("===========================");
    print_endline("Treating stdin");
    go(MyParser.parse(IO.read_from_stdin()))

  let treatfile filename = print_endline("===========================");
    print_endline("Treating file "^filename);
    go(MyParser.parse(IO.read_from_file filename))

  let treatdir a = print_endline("Treating directory "^a);
    let b = Sys.readdir(a) in
      Array.sort Pervasives.compare b;
      for i=0 to Array.length b-1 do
	let _ = treatfile (a^Filename.dir_sep^b.(i)) in ();
      done

  let rec treatexamples = function
    | []   -> ""
    | a::l -> (print_test a)^(treatexamples l)

end
