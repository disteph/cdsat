(*******************************)
(* Main entry point for Kernel *)
(*******************************)

open General
open Sums
open Top
open Messages
open Sassigns

(* Extended API *)
module type APIext = sig
  include Combo.API
  (* Also identifies the problem we are trying to solve as an input assignment *)
  val problem    : Assign.t
  (* Expected answer if the above if known *)
  val expected   : bool option

  (* Below:
     checks a CDSAT definitive answer against the problem we are supposed to solve *)
  type answer = private
    | UNSAT of unsat WB.t
    | SAT of Assign.t
    | NotAnsweringProblem

  val answer : (unsat WB.t, WB.sat_ans) sum -> answer
end
