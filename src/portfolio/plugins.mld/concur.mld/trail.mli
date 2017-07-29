(*********************)
(* Conflict analysis *)
(*********************)

open Async

open Kernel
open Top.Messages

open General
open Patricia_interfaces
open Sums

open Interfaces
       
(* This module implements conflict analysis *)

module Make(WB : WhiteBoardExt) : sig

  open WB.DS

  (* type nature indicates the status of each formula accumulated in the trail so far.
The reason it was added to it was either:
- It was in the original problem
- It was propagated from other formulae already present in the trail
- It was a decision *)
                             
  type nature =
    | Input
    | Deduction of straight WB.t
    | Decision
[@@deriving show]

  type t

  val init : t
  val add: nature:nature
           -> sassign
           -> t
           -> t

  val level: t -> int
  val chrono: t -> int
                
  val analyse : t             (* the trail *)
                -> unsat WB.t (* the conflict *)
                -> (unsat WB.t -> WB.sassign -> unit Deferred.t)
                (* a function to which we can pass stuff to learn *)
                -> (unsat WB.t,  (* Either a subset of the original formulae are unsat *)
                    int * straight WB.t list) (* Or there is a level to backjump to,
                                              with propagation messages to use for the backjump branch *)
                     sum Deferred.t
                           
end
