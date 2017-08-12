(*****************)
(* Message types *)
(*****************)

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
type (_,_) propagated =
 private
  | Unsat    : (_,unsat_l) propagated           (* concluding ⊥ (= conflict) *)
  | Straight : 'b -> ('b,straight_l) propagated (* concluding a proper Boolean assignment *)
                                                 
type (_,_,_) message =
  private
  (* Message saying a theory module is happy with assignment assign,
     sharing the set of terms sharing, the Σ-variables of assign being myvars;
     This is called "T0-compatibility of [assign] sharing [sharing]" in the CDSAT papers. *)
  | Sat   : { assign : 'j; sharing:'tset; myvars:'tset Lazy.t } -> (_,'j*_*'tset,sat) message
  (* Propa(H,A) is the theory inference (a.k.a. propagation) H⊢A *)
  | Propa : 'j * ('b,'l) propagated -> (_,'j*'b*_,'l propa) message

(* Message construction functions *)
val sat     : 'sign -> 'j -> sharing:'tset -> myvars:'tset Lazy.t -> ('sign,'j*_*'tset,sat) message
val propa   : 'sign -> 'j -> ('b,'l) propagated -> ('sign,'j*'b*_,'l propa) message
val unsat   : 'sign -> 'j                       -> ('sign,'j*_*_,unsat) message
val straight: 'sign -> 'j -> 'b                 -> ('sign,'j*'b*_,straight) message

(* Pretty-printing messages *)
val print_msg_in_fmt: (Format.formatter -> 'j -> unit)
                      -> (Format.formatter -> 'b -> unit)
                      -> (Format.formatter -> 'tset -> unit)
                      -> Format.formatter -> (_,'j*'b*'tset,_)message -> unit
