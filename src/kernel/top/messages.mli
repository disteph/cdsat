(*****************)
(* Message types *)
(*****************)

(* Type labels, used in GADTs *)

type unsat_l    = private CUnsat
type straight_l = private CStraight
type both_l     = private CBoth
type either_l   = private CEither
type 'a propa = private CPropa
type sat      = private CSat

(* Abbreviation for type labels, used in GADTs *)
                          
type unsat    = unsat_l propa
type straight = straight_l propa
type both     = both_l propa
type either   = either_l propa

(* Message types *)
                         
type (_,_) propagated =
  | Unsat    : (_,unsat_l) propagated
  | Straight : 'tset -> ('tset,straight_l) propagated
  | Both     : 'tset * 'tset -> ('tset,both_l) propagated
  | Either   : 'tset * 'tset -> ('tset,either_l) propagated
                                                 
type (_,_,_) message =
  | Sat   : 'tset -> (_,'tset,sat) message
  | Propa : 'tset * ('tset,'a)propagated -> (_,'tset,'a propa) message

(* Message construction functions *)
                                                                  
val sat     : 'sign -> 'tset -> ('sign,'tset,sat) message
val propa   : 'sign -> 'tset -> ('tset,'a) propagated -> ('sign,'tset,'a propa) message
val unsat   : 'sign -> 'tset -> ('sign,'tset,unsat) message
val straight: 'sign -> 'tset -> 'tset      -> ('sign,'tset,straight) message
val both    : 'sign -> 'tset -> 'tset -> 'tset -> ('sign,'tset,both) message
val either  : 'sign -> 'tset -> 'tset -> 'tset -> ('sign,'tset,either) message

(* Printing messages *)

val print_msg_in_fmt: (Format.formatter -> 'tset -> unit) -> Format.formatter -> (_,'tset,_)message -> unit
