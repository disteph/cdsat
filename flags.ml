let memo  = ref true   (* For using Memoisation in MyPatricia *)
let almo  = ref true   (* For using almost in Memoisation in MyPatricia *)
let unitp = ref true   (* Eager Unit propagate in MyPatricia *)

let debug = ref 0          (* Activates debug mode (displays fails, etc) *)
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
      1000;
      (* thing to memorise is weaker than current knowledge *)
      100000;
      (* cut *)
      1;
      (* focus and cie *) 
      1000
    |] 
let weakenings = ref true  (* Activates a-posteriori weakening mode
			      (irrelevant formulae are not in proof-tree) *)

let do_file = Some("test.cnf")
let do_dir  = None (* Some("problems/sat/RTI_k3_n100_m429") *)
