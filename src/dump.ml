(********************************************************)
(* This module is where the different Psyche components *)
(* can dump information during runs, which can then be  *)
(* printed.                                             *)
(********************************************************)

let every
    = [|(* local success *)
      0;
      (* local failure *)
      0; 
      (* fakefail success *)
      0;
      (* fakefail failure *)
      0;
      (* memorised new thing *)
      0;
      (* thing to memorise is weaker than current knowledge *)
      0;
      (* cut *)
      0;
      (* focus and cie *) 
      0;
      (* time display, set to negative if you want no display *) 
      0
    |] 

let msg un deux = function
  | Some i when Flags.every.(i)==0||((every.(i) mod Flags.every.(i)) !=0)
      -> ()
  | _ -> let message = ref un in
         (if !Flags.debug>1
          then match deux with
          | Some _ -> message :=deux
          | None   ->());
         match !message with
         | Some a -> print_endline a;
         | None   ->()

(********************)
(* Kernel dump area *)
(********************)

module Kernel = struct

  let start_time    = ref (Sys.time())
  let last_display  = ref (Sys.time())

  (* Array where we count how many events we get *)
  let count = [|0;0;0;0;0;0;0;0;0;0|]

  let incr_count i = count.(i) <- count.(i) + 1
  let read_count i = count.(i)
  let incr_branches() = count.(6) <- count.(6) + 1
  let decr_branches() = count.(6) <- count.(6) - 1

  let init() = incr_branches();start_time:=Sys.time();last_display:=!start_time

  let print_state i = 
    ("With "
     ^(string_of_int count.(0))^" Successes, "
     ^(string_of_int count.(1))^" Failures, "
     ^(string_of_int count.(7))^" Loops detected, "
     ^(string_of_int count.(8))^" Notifies, "
     ^(string_of_int count.(4))^" Focus, "
     ^(string_of_int count.(5))^" Cuts, and "
     ^(string_of_int count.(2))^" Fake successes and "
     ^(string_of_int count.(3))^" Fake failures, "
     ^(string_of_int count.(9))^" operations"
     ^(if i>0 then (", "^string_of_int count.(6)^" open branches") else ".")
    )

  (* Print Kernel's timely report *)
  let print_time() =
    let b=Sys.time() in
    let c=b-. !last_display  in
    if c>float_of_int Flags.every.(8) then
      (last_display:=b;
       print_endline(string_of_int (int_of_float(b-. !start_time))^" seconds");
       print_endline(print_state 1))

  (* Print Kernel's final report *)
  let report w = 
    print_endline("   Kernel's report:");
    print_endline(w
		  ^", in "
		  ^string_of_float (Sys.time()-. !start_time)
		  ^" seconds");
    print_endline(print_state 0)

  let clear() = for i=0 to Array.length count-1 do count.(i) <- 0 done

end

(********************)
(* Plugin dump area *)
(********************)

module Plugin = struct

  (* Array where we count how many events we get *)
  let count = [|0;0;0;0;0;0;0;0;0|]

  let incr_count i = count.(i) <- count.(i) + 1
  let read_count i = count.(i)

  let clear() = for i=0 to Array.length count-1 do count.(i) <- 0 done

end
