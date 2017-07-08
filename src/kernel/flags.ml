(* This module is the control panel to run the program with different options *)
(* Dump flags *)

let latex    = ref false   (* Activates latex output *)

(* Plotting *)
let plot = ref false (* Produce debug output in a form suitable for plotting *)

(* Periodicity of events *)

let every                  (* Prints every XXX events *)
    = [|(* local success *)
      100000;
      (* local failure *)
      100000; 
      (* fakefail success *)
      1;
      (* fakefail failure *)
      1;
      (* memorised new thing *)
      1;
      (* thing to memorise is weaker than current knowledge *)
      100000;
      (* cut *)
      100;
      (* focus and cie *) 
      1000;
      (* time display, set to negative if you want no display *) 
      10
    |] 

(* Kernel flags *)

let printrhs   = ref false (* When printing a sequent, print right-hand side? *)
let cuts       = ref true  (* Allows cuts *) 
let depol      = ref true  (* Allows depolarisation *)
let fair       = ref false (* Ensures fairness between formulae for focus *)
let weakenings = ref true  (* Activates a-posteriori weakening mode
			      (irrelevant formulae are not in proof-tree) *)

(* Portfolio flags *)

(* MyPatricia flags *)

let memo  = ref true   (* For using Memoisation in MyPatricia -currently has to be true *)
let almo  = ref true   (* For using almost in Memoisation in MyPatricia *)
let unitp = ref true   (* Eager Unit propagate in MyPatricia *)

(* Plugin flags *)
let restarts_strategy = ref "none" (* Allows restarts. Currently only implemented in DPLL_WL *) 
let restarts_p1 = ref 10 (* First restart strategy parameter *)
let restarts_p2 = ref 0  (* Second restart strategy parameter *)

