(* Main file *)

(* Choosing theory: *)

(* open Empty *)
open Lia

(* Choosing plugin: *)
open DPLL_WL

open Run_tools.IO

(* The following line loads the architecture:
Kernel + Theory + Plugin 
Generic plugin is to be generated from the theory,
with its type "literals" being the same as type Atom.t from theory
*)
include Run_tools.Test.Prepare(MyTheory)(MyPlugin.GenPlugin(MyTheory));;

(* Main function:
If command-line argument is given, it is considered as the name of the file or directory to treat (using the parser of MyTheory).
If not, the examples of formulae given in MyTheory are treated and the output is produced in latex format.
*)
if (Array.length Sys.argv) = 1 then
  let _ = treatstdin() in ()
(*  write_to_file "latex/output.tex" (treatexamples MyParser.examples) *)
else
  let a = Sys.argv.(1) in
    if Sys.is_directory a
    then let _ = treatdir a in ()
    else let _ = treatfile a in ()
;;
