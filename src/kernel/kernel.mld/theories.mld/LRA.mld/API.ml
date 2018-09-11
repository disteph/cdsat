open General
open Patricia
open Patricia_tools
open Sums       
       
open Top
open Basic
open Messages
open Terms
open Values
open Sassigns

open Termstructures

module type API = sig
  type sign

  (* Conversion functions between LRA values and global values *)
  val vinj : Q.t -> Value.t
  val vproj: Value.t -> Q.t option

  (* Map from variables to rational values (justified by an single assignment).
     Used for valuations below. *)
  module QMap : Map.S_NH with type keys = Term.t
                          and type values = Q.t * SAssign.t
                          and type common = int
                          and type branching = int

  (* Module for LRA valuations:
     essentially, they are the above maps, but protected *)
  module Model : sig
    type t
    val empty : t
    val add : SAssign.t -> t -> t
    val map : t -> QMap.t
  end

  (* Module for simplified terms: the original term is always kept,
     and its LRA canonical form can be incrementally simplified by function simplify.
     The LRA canonical form can be queried by functions scaling, nature, coeffs, constant.
     justif returns the assignments that have participated to the simplification so far.
     watchable maintains a list of at most 2 variables that are not determined. *)
  module Simpl : sig
    type t [@@deriving show]
    val term    : t -> Term.t
    val scaling : t -> Q.t
    val nature  : t -> Rationals.nature
    val coeffs  : t -> Termstructures.Rationals.VarMap.t
    val constant  : t -> Q.t
    val watchable : t -> Term.t list
    val justif  : t -> Assign.t
    val simplify: Model.t-> t-> t
    val make    : Term.t -> t
  end

  type state
  val init : state

  (* Output type for the evaluation function below *)
  type eval =
    (* Term of sort Bool evaluates to b=true or false, we produce a message J⊢(t↦b) *)
    | Beval   of (sign, straight) message
    (* Term of sort Q evaluates to q, we produce the pair (t,q) *)
    | Qeval   of Term.t * Q.t
    (* Term of sort Bool would evaluate, were it not for the unassigned variable var.
       This gives a bound for var. Whether this is an upper, lower, or double bound,
       whether it is large or strict, etc, can be determined by nature
       and the sign of the coefficient for var in the term *)
    | Unit    of { var         : Term.t;
                   nature      : Rationals.nature;
                   is_coeff_pos: bool;
                   bound       : Q.t }
    (* There are at least 2 unassigned variables in the term.
       Cannot evaluate it in any meaningful way,
       but you can watch the variables in the list I give you, of length at least 2. *)
    | ToWatch of Term.t list

  exception IdontUnderstand

  (* Evaluates a simplified term c,
     special cases if all variables are assigned or ony one is missing *)
  val eval  : Simpl.t -> eval

  (* Fourier-Motzkin resolution of ba1 and ba2 over variable var
     Creates message ba1,ba2 ⊢ FM_resolvant(ba1,ba2) *)
  val fm    : bassign -> bassign -> Term.t -> (sign, straight) message

  (* disequal (lower≤x) (x≠diseq) (x≤upper) x
     generates (lower=diseq), (diseq=upper),
     and the message (lower≤x),(x≠diseq),(x≤upper),(lower=diseq),(diseq=upper)⊢ ⊥ *)
  val disequal : bassign -> bassign -> bassign -> Term.t
                 -> Term.t * Term.t * (sign, unsat) message

  (* Outputs sat message if all terms to satisfy/evaluate in state have been so. *)
  val sat   : Model.t -> state -> state * (sign, sat) message option

  (* Adds a single assignment to the state, return a new state.
     If a new constraint has to be satisfied, it is returned as well. *)
  val add   : SAssign.t -> state -> state * (Simpl.t * Q.t Values.values) option

  (* Adds to the state a set of terms to share, returning a new state.
     If new terms need values, they are returned as well. *)
  val share : TSet.t -> state -> state * Simpl.t list
                                                
end
