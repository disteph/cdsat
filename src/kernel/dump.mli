(********************************************************)
(* This module is where the different Psyche components *)
(* can dump information during runs, which can then be  *)
(* printed.                                             *)
(********************************************************)

open General

(**********************)
(* Printing functions *)
(**********************)

type display =
  | Latex
  | Utf8

val display : display ref
                      
val every : int array
        
(**********)
(* Timers *)
(**********)

val gtimer : Timer.t
val ltimer : Timer.t
val ktimer : Timer.t
val ptimer : Timer.t
val ttimer : Timer.t

val plot : string -> int -> unit

(********************)
(* Kernel dump area *)
(********************)

module Kernel : sig
  (* Array where we count how many events we get *)
  val count : int array
  val count_labels : string array
  val read_count : int -> int
  val incr_count : int -> unit
  val reset_branches : unit -> unit
  val incr_branches : unit -> unit
  val decr_branches : unit -> unit
  val fromPlugin : unit -> unit
  val toPlugin : unit -> unit
  val fromTheory : unit -> unit
  val toTheory : unit -> unit
  val init : unit -> unit
  val print_state : int -> string
  val print_time : unit -> unit
  val report : string -> unit
  val clear : unit -> unit
end

(********************)
(* Plugin dump area *)
(********************)

module Plugin : sig
  val count : int array
  val count_labels : string array
  val incr_count : int -> unit
  val read_count : int -> int
  val clear : unit -> unit
end
