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
  | Some i (* when Flags.every.(i)==0||((every.(i) mod Flags.every.(i)) !=0) *)
      -> ()
  | _ -> let message = ref un in
         (if !Flags.debug>1
          then match deux with
          | Some _ -> message :=deux
          | None   ->());
         match !message with
         | Some a -> print_endline a;
         | None   ->()

(**********)
(* Timers *)
(**********)

module Timer = (struct

  type t = string*(float ref)*(float option ref)

  let newtimer s = 
    let timed   = ref 0. in
    let ongoing = ref None in
    (s,timed,ongoing)

  let name (s,_,_) = s

  let isRunning (s,timed,ongoing) =
    match ongoing with 
    | Some _ -> true
    | None   -> false

  let start (s,timed,ongoing) =
    match !ongoing with
    | Some last -> failwith("Trying to start timer "^s^" but it is already started")
    | None      -> ongoing := Some(Sys.time())

  let stop (s,timed,ongoing) =
    match !ongoing with
    | Some last -> let span = Sys.time()-.last in
                   timed := !timed+.span;ongoing := None
    | None -> failwith("Trying to stop timer "^s^" but it is already stopped")

  let watch (s,timed,ongoing) =
    match !ongoing with
    | Some last -> let span = Sys.time()-.last in !timed+.span
    | None -> !timed

  let reset (s,timed,ongoing) =
    match !ongoing with
    | Some last -> timed:=0.;ongoing:=Some(Sys.time())
    | None -> timed:=0.

  let transfer t t' = stop t; start t'

end: sig
  type t
  val newtimer:string->t
  val start: t->unit
  val stop: t->unit
  val name: t->string
  val watch: t->float
  val reset: t->unit
  val transfer: t->t->unit
end)

let gtimer = Timer.newtimer "General"
let ltimer = Timer.newtimer "Last"
let ktimer = Timer.newtimer "Kernel"
let ptimer = Timer.newtimer "Plugin"
let ttimer = Timer.newtimer "Theory"

(********************)
(* Kernel dump area *)
(********************)

module Kernel = struct

  let fromPlugin() = Timer.transfer ptimer ktimer
  let toPlugin  () = Timer.transfer ktimer ptimer
  let fromTheory() = Timer.transfer ttimer ktimer
  let toTheory  () = Timer.transfer ktimer ttimer

  (* Array where we count how many events we get *)
  let count = [|0;0;0;0;0;0;0;0;0;0|]

  let incr_count i = count.(i) <- count.(i) + 1
  let read_count i = count.(i)
  let incr_branches() = count.(6) <- count.(6) + 1
  let decr_branches() = count.(6) <- count.(6) - 1

  let init() = incr_branches();
    Timer.reset gtimer;Timer.start gtimer;
    Timer.reset ltimer;Timer.start ltimer;
    Timer.reset ktimer;Timer.start ktimer

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
    if Timer.watch ltimer>float_of_int Flags.every.(8) then
      (Timer.reset ltimer;
       print_endline(string_of_int (int_of_float(Timer.watch gtimer))^" seconds");
       print_endline(print_state 1))

  (* Print Kernel's final report *)
  let report w = 
    Timer.stop gtimer;
    Timer.stop ltimer;
    Timer.stop ktimer;
    print_endline("   Kernel's report:");
    print_endline(w
		  ^", in "
		  ^string_of_float (Timer.watch gtimer)
		  ^" seconds ("
		  ^string_of_int(int_of_float(100.*.(Timer.watch ktimer)/.(Timer.watch gtimer)))
                  ^"% in kernel, "
		  ^string_of_int(int_of_float(100.*.(Timer.watch ptimer)/.(Timer.watch gtimer)))
                  ^"% in plugin, "
		  ^string_of_int(int_of_float(100.*.(Timer.watch ttimer)/.(Timer.watch gtimer)))
                  ^"% in theory).");
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
