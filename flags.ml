let memo = ref true   (* For using Memoisation in MyPatricia *)
let almo = ref true   (* For using almost in Memoisation in MyPatricia *)
let unitp = ref true (* Eager Unit propagate in MyPatricia *)

let debug = ref 1          (* Activates debug mode (displays fails, etc) *)
let printrhs = ref false
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
    100;
  (* thing to memorise is weaker than current knowledge *)
    100000;
  (* cut *)
    10;
(* focus and cie *) 
    1000
|] 
let loop_detect = ref true (* Activates loop detection *)
let weakenings = ref true  (* Activates a-posteriori weakening mode
				(irrelevant formulae are not in proof-tree) *)

let do_file = None (* Some("test.cnf") *)
let do_dir  = Some("problems/sat/uf20-91")
