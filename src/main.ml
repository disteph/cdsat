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

open Flags
open Run_tools.IO
open Run_tools.Test
open Arg

(* Deals with command line arguments *)
let options =
  ("-theory",        String(fun s->
			      mytheory:=
				Some(try ThDecProc_register.getbyname s
				     with ThDecProc_register.NotFound msg
					 -> failwith msg)),"selects theory (among empty, lra)")::
    ("-gplugin",     String(fun s->
			      mygplugin:=
				Some(try GPlugins_register.getbyname s
				     with GPlugins_register.NotFound msg
					 -> failwith msg)),"selects generic plugin (among naive, dpll_pat, dpll_wl)")::
    ("-latex",       Unit(fun()->latex:=true),        "allows latex output")::
    ("-examples",    Unit(fun()->texamples:=true),    "treats theory examples instead of standard input")::
    ("-skipsat",     Unit(fun()->skipsat:=true),      "skips instances expected to be sat")::
    ("-skipunsat",   Unit(fun()->skipunsat:=true),    "skips instances expected to be unsat")::
    ("-skipunprovable",Unit(fun()->skipsat:=true),    "skips instances expected to be unprovable")::
    ("-skipprovable",  Unit(fun()->skipunsat:=true),  "skips instances expected to be provable")::
    ("-skipunknown", Unit(fun()->skipunknown:=true),  "skips instances without any result expectation")::
    ("-nocuts",      Unit(fun()->cuts:=false),        "disallows cuts")::
    ("-nodepol",     Unit(fun()->depol:=false),       "disallows depolarisation of literals")::
    ("-fair",        Unit(fun()->fair:=true),         "ensures fairness between formulae for focus")::
    ("-noweakenings",Unit(fun()->weakenings:=false),  "disallows conflict analysis")::
    ("-debug",       Int(fun i->debug:=i),            "decides level of debug printing (0,1,2); 0 is no debug")::
    ("-printrhs",    Unit(fun ()->printrhs:=true),    "when debug printing allowed, prints right-hand sides of sequents")::
    ("-nomemo",      Unit(fun ()->memo:=false),       "disallows memoisation")::
    ("-noalmo",      Unit(fun ()->almo:=false),       "memoisation is done on exact match only")::
    ("-nounitpropagation",Unit(fun()->printrhs:=true),"disallows eager unit propagation in MyPatricia")::
    []

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
let (trname,trexamples,trstdin)=treatprimitives in
let rec treat = function
  | []      -> ()
  | name::l -> trname name; treat l
in
  match !fname with
    | [] when !texamples-> trexamples ()
    | []                -> trstdin    ()
    | l                 -> treat (List.rev l)
