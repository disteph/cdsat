let memo = ref true   (* For using Memoisation in MyPatricia *)
let almo = ref true   (* For using almost in Memoisation in MyPatricia *)

let debug = ref 0          (* Activates debug mode (displays fails, etc) *)
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
    10;
|] 
let loop_detect = ref true (* Activates loop detection *)
let weakenings = ref true  (* Activates a-posteriori weakening mode
				(irrelevant formulae are not in proof-tree) *)

let do_file = Some("test.cnf")
let do_dir  = None (* Some("problems/sat/flat50-115") *)
