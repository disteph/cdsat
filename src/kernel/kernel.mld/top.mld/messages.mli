(*****************)
(* Message types *)
(*****************)
open Terms
open Sassigns

(* Type labels, used in GADTs *)
type unsat_l    = private CUnsat
type straight_l = private CStraight
type 'a propa = private CPropa
type sat      = private CSat

(* Abbreviation for type labels, used in GADTs *)                          
type unsat    = unsat_l propa
type straight = straight_l propa

(* Message types *)

(* Type of things that are the conclusion of theory inferences *)
type _ propagated = private
  | Unsat    : unsat_l propagated               (* concluding ⊥ (= conflict) *)
  | Straight : BAssign.t -> straight_l propagated (* concluding a proper Boolean assignment *)

type (_,_) message = private
  (* Message saying a theory module is happy with assignment assign,
     sharing the set of terms sharing, the Σ-variables of assign being myvars;
     This is called "T0-compatibility of [assign] sharing [sharing]" in the CDSAT papers. *)
  | Sat   : { assign : Assign.t; sharing: TSet.t; myvars: TSet.t Lazy.t} -> (_,sat) message
  (* Propa(H,A) is the theory inference (a.k.a. propagation) H⊢A *)
  | Propa : Assign.t * 'l propagated -> (_,'l propa) message

(* Message construction functions *)
val sat     : 'sign -> Assign.t -> sharing:TSet.t -> myvars:TSet.t Lazy.t -> ('sign,sat) message
val propa   : 'sign -> Assign.t -> 'l propagated -> ('sign,'l propa) message
val unsat   : 'sign -> Assign.t                  -> ('sign,unsat) message
val straight: 'sign -> Assign.t -> BAssign.t     -> ('sign,straight) message

(* Pretty-printing messages *)
val pp_message: (_,_) message Format.printer
