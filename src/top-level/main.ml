(************************************************************************)
(*                                                                      *)
(*     CDSAT                                                            *)
(*                                                                      *)
(*     Stéphane Graham-Lengrand                                         *)
(*                                                                      *)
(*     CNRS - SRI International - INRIA - Ecole Polytechnique           *)
(*                                                                      *)
(*   This file is distributed under the terms of the CeCILL-C licence   *)
(*   Some plugins use some (unmodified) code                            *)
(*   from the Alt-Ergo theorem prover (distributed under CeCILL-C)      *)
(*                                                                      *)
(************************************************************************)
(* Main file *)

open Arg

open General
open Flags
open PFlags
open TopFlags
open IO
open Top_level

(* Deals with command line arguments *)
let options =
  [
    ("-no",          String(fun s-> 
                         match !notheories with
                         | None   -> ()
                         | Some l -> notheories:= Some(s::l)), "XXX forbids use of theory XXX");
    ("-th",          String(fun s-> 
                         match !notheories with
                         | None   -> ()
                         | Some l -> addtheories:= Some(s::l)), "XXX forces use of theory XXX");
    ("-alltheories", Unit(fun ()-> addtheories:= None), "makes Psyche use all theories, except those specified by -no option");
    ("-notheories",  Unit(fun ()-> notheories := None), "makes Psyche use no theories, except those specified by -th option");
    ("-plugin",      Set_string myplugin,             "XXX selects XXX as the main plugin (among async)");
    ("-parser",      String(fun s-> 
                         match !parser with
                         | None   -> parser:= Some [s]
                         | Some l -> parser:= Some(s::l)),    "XXX authorises XXX among the parsers to try (among dimacs, smtlib2)");
    ("-latex",       Unit(fun()->latex:=true),        "allows latex output");
    ("-alphasort",   Clear sizesort,                  "treats input files in alphabetical order (default is from smaller to bigger)");
    ("-remember",    Clear clear4each,                "remembers tables (hash-consing & learnt lemmas) between each problem (default is no)");
    ("-skipsat",     Set skipsat,                     "skips instances expected to be sat");
    ("-skipunsat",   Set skipunsat,                   "skips instances expected to be unsat");
    ("-skipunprovable",Set skipsat,                   "skips instances expected to be unprovable");
    ("-skipprovable",  Set skipunsat,                 "skips instances expected to be provable");
    ("-skipunknown", Set skipunknown,                 "skips instances without any result expectation");
    ("-bool_decay",  String(fun s-> 
                         bool_decay := float_of_string s), "sets the VSIDS decay factor (default 1.3)");
    ("-keeplemmas", Clear forgetlemmas, "does not forget any lemma");
    ("-lemmasincrmt",  String(fun s-> 
                           lemmasincrmt := float_of_string s), "sets the lemmas increment factor (default 1.1)");
    ("-lemmasmax",  String(fun s-> 
                        lemmasmax := int_of_string s), "sets the max lemma count (default 1000)");
    ("-debug",       Tuple[
                         Set_string dtag;
                         Int(fun i-> dtags:=(!dtag,i,false)::!dtags)],
                                                      "XXX iii prints on stdout those debug messages whose tags contain XXX and whose level is at most iii");
    ("-step",        Unit(fun ()->
                         match !dtags with
                         | [] -> failwith "-step option only makes sense after the -debug option"
                         | (dtag,i,_)::next->dtags:=(dtag,i,true)::next),
                                                      "waits for keystroke after printing each debug message (applies to latest -debug)");
    ("-nomemo",      Clear memo,                      "disallows memoisation");
    ("-version", Unit(fun ()-> print_endline Version.version_string; Pervasives.exit 0), "prints version and exits")
  ]

let description =
"=====================================================================
This is CDSAT
It is distributed under the CeCILL-C licence, see
http://www.cecill.info/index.en.html
=====================================================================

A command-line argument will be considered as the file name or directory name to treat.
If no argument is given, the standard input will be used.

Available options are:";;



Arg.parse options (fun a->fname:= a::!fname) description;
Print.init !dtags;
let trname, trstdin = treatprimitives() in
let rec treat = function
  | []      -> ()
  | name::l -> trname name; treat l
in
match !fname with
| []                -> trstdin    ()
| l                 -> treat (collect_sort l)
