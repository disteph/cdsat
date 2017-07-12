(* This module is the control panel to run the program with different options *)
(* Dump flags *)

let latex = ref false   (* Activates latex output *)

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


(* Plugin's flag *)
let memo = ref true
