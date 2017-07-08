(********************************************************)
(* This module is where the different Psyche components *)
(* can dump information during runs, which can then be  *)
(* printed.                                             *)
(********************************************************)

val init : (string * int * bool) list -> unit

(**********************)
(* Printing functions *)
(**********************)

type display =
  | Latex
  | Utf8

val display : display ref

val toString : ((('a, Format.formatter, unit) format -> 'a) -> 'b) -> string
val stringOf : (Format.formatter -> 'a -> unit) -> 'a -> string
val print : (string * int) list -> ((('a, Format.formatter, unit) format -> 'a) -> 'b) -> unit
                      
val wait : unit -> unit
                
val every : int array
        
(**********)
(* Timers *)
(**********)

module Timer : sig
  type t
  val newtimer:string->t
  val start: t->unit
  val stop: t->unit
  val name: t->string
  val watch: t->float
  val reset: t->unit
  val transfer: t->t->unit
end

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
