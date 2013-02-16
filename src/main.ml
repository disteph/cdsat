(* Main file *)

open Kernel
open Run_tools
open Flags
open Formulae
open Test

include Empty
module MyPlugin = DPLL_Pat.MyPlugin.GenPlugin(MyTheory)

include Tests(MyTheory)(MyPlugin)

let rec treat_ex = function
  | []   -> ""
  | a::l -> (print_test a)^(treat_ex l);;

if (Array.length Sys.argv) = 1 then
  write_to_file "latex/output.tex" (treat_ex MyParser.examples)
else
  let a = Sys.argv.(1) in
    if Sys.is_directory a
    then let _ = treatdir a in ()
    else let _ = treatfile a in ()
;;
