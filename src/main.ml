(**************************************************************************)
(*                                                                        *)
(*     Psyche,                                                            *)
(*     the Proof-Search factorY for Collaborative HEuristics              *)
(*     Copyright (C) 2012-2013                                            *)
(*                                                                        *)
(*     Stéphane Graham-Lengrand                                           *)
(*     Assia Mahboubi                                                     *)
(*                                                                        *)
(*     Alexis Bernadet                                                    *)
(*     Mahfuza Farooque                                                   *)
(*     Zelda Mariet                                                       *)
(*     Clément Pit-Claudel                                                *)
(*     Damien Rouhling                                                    *)
(*     Matthieu Vegreville                                                *)
(*                                                                        *)
(*     CNRS - INRIA - Ecole Polytechnique                                 *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*   Some plugins use some (unmodified) code                              *)
(*   - from the Alt-Ergo theorem prover (distributed under CeCILL-C)      *)
(*   - written by Pierre-Yves Strub                                       *)
(*                                                                        *)
(**************************************************************************)
(* Main file *)

open Arg

open Flags
open IO
open Top_level

(* Deals with command line arguments *)
let options =
  [
    ("-no",          String(fun s-> 
                         match !notheories with
                         | None   -> notheories:= Some [s]
                         | Some l -> notheories:= Some(s::l)), "XXX forbids theory XXX");
    ("-alltheories", Unit(fun ()-> notheories:= Some []), "forces all theories to be used (regardless of parsed input)");
    ("-fo",          Clear mode,                      "selects the first-order sequent calculus mode");
    ("-plugin",      Set_string myplugin,             "XXX selects XXX as the main plugin (among async)");
    ("-pluginG",     Set_string mypluginG,            "XXX selects XXX as the pure logic plugin (among naive, hint, dpll_pat, dpll_wl)");
    ("-parser",      String(fun s-> 
                         match !parser with
                         | None   -> parser:= Some [s]
                         | Some l -> parser:= Some(s::l)),    "XXX authorises XXX among the parsers to try (among dimacs, smtlib2)");
    ("-latex",       Unit(fun()->latex:=true;printrhs:=true), "allows latex output");
    ("-alphasort",   Clear sizesort,                  "treats input files in alphabetical order (default is from smaller to bigger)");
    ("-skipsat",     Set skipsat,                     "skips instances expected to be sat");
    ("-skipunsat",   Set skipunsat,                   "skips instances expected to be unsat");
    ("-skipunprovable",Set skipsat,                   "skips instances expected to be unprovable");
    ("-skipprovable",  Set skipunsat,                 "skips instances expected to be provable");
    ("-skipunknown", Set skipunknown,                 "skips instances without any result expectation");
    ("-nocuts",      Clear cuts,                      "disallows cuts");
    ("-nodepol",     Clear depol,                     "disallows depolarisation of literals");
    ("-fair",        Set fair,                        "ensures fairness between formulae for focus");
    ("-noweakenings",Clear weakenings,                "disallows conflict analysis");
    ("-debug",       Tuple[
                         Set_string dtag;
                         Int(fun i-> dtags:=(!dtag,i,false)::!dtags)],
                                                      "XXX iii prints on stdout those debug messages whose tags contain XXX and whose level is at least iii");
    ("-step",        Unit(fun ()->
                         match !dtags with
                         | [] -> failwith "-step option only makes sense after the -debug option"
                         | (dtag,i,_)::next->dtags:=(dtag,i,true)::next),
                                                      "waits for keystroke after printing each debug message (applies to latest -debug)");
    ("-printrhs",    Set printrhs,                    "when debug printing allowed, prints right-hand sides of sequents");
    ("-nomemo",      Clear memo,                      "disallows memoisation");
    ("-noalmo",      Clear almo,                      "memoisation is done on exact match only");
    ("-nounitpropagation", Set printrhs,              "disallows eager unit propagation in MyPatricia");
    ("-restarts",    Set_string restarts_strategy,    "select a restart stratedy (from constant, arithmetic, geometric, exponential, luby, and none; default: none; DPLL_WL specific)");
    ("-rsettings",   Tuple [Set_int restarts_p1; Set_int restarts_p2], "fine-tunes the restart strategy, specifying the initial restart threshold and a strategy-specific setting (defaults: 10 0; some strategies discard the second parameter; DPLL_WL specific)");
    ("-plot",        Set plot,                        "prints large amounts of debug data, formatted for input to a plotting script");
    ("-version", Unit(fun ()-> print_endline Version.version_string; Pervasives.exit 0), "prints version and exits")
  ]

let description =
"=====================================================================
This is Psyche, the Proof-Search factorY for Collaborative HEuristics
http://www.lix.polytechnique.fr/~lengrand/Psyche/
Copyright (C) CNRS-INRIA-Ecole Polytechnique
It is distributed under the CeCILL-C licence, see
http://www.cecill.info/index.en.html
=====================================================================

A command-line argument will be considered as the file name or directory name to treat.
If no argument is given, the standard input will be used.

Available options are:";;


(* Main function:
   The following lines load the architecture:
   Kernel + Theory + Plugin 
   Generic plugin is to be generated from the theory,
   with its type "literals" being the same as type Atom.t from theory
*)

Arg.parse options (fun a->fname:= a::!fname) description;
Dump.init();
let trname, trstdin = treatprimitives() in
let rec treat = function
  | []      -> ()
  | name::l -> trname name; treat l
in
  match !fname with
    | []                -> trstdin    ()
    | l                 -> treat (collect_sort l)
