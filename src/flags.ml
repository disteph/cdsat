(* This module is the control panel to run the program with different options *)

(* Kernel flags *)

let weakenings = ref true  (* Activates a-posteriori weakening mode
			      (irrelevant formulae are not in proof-tree) *)

let debug    = ref 0       (* Activates debug mode (displays fails, etc) *)
let printrhs = ref false   (* When printing a sequent, print right-hand side? *)

(* MyPatricia flags *)

let memo  = ref true   (* For using Memoisation in MyPatricia -currently has to be true *)
let almo  = ref true   (* For using almost in Memoisation in MyPatricia *)
let unitp = ref true   (* Eager Unit propagate in MyPatricia *)

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
      1000;
      (* thing to memorise is weaker than current knowledge *)
      100000;
      (* cut *)
      100;
      (* focus and cie *) 
      1000;
      (* time display, set to negative if you want no display *) 
      10
    |] 

