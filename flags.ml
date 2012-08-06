let treeHCons = ref true   (* HConsing the Patricia trees in myPatricia *)
let debug = ref 1          (* Activates debug mode (displays fails, etc) *)
let every = [|1;1;1;1|] (* Prints every XXX events *)
let loop_detect = ref true (* Activates loop detection *)
let weakenings = ref true  (* Activates a-posteriori weakening mode
				(irrelevant formulae are not in proof-tree) *)
let do_file = ref false


