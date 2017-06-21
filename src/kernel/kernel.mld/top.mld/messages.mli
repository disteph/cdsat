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
  | Straight : 'b -> ('b,straight_l) propagated
  | Both     : 'b * 'b -> ('b,both_l) propagated
  | Either   : 'b * 'b -> ('b,either_l) propagated
                                                 
type (_,_,_) message =
  | Sat   : 'j -> (_,'j*_,sat) message
  | Propa : 'j * ('b,'l) propagated -> (_,'j*'b,'l propa) message

(* Message construction functions *)
                                                                  
val sat     : 'sign -> 'j -> ('sign,'j*_,sat) message
val propa   : 'sign -> 'j -> ('b,'l) propagated -> ('sign,'j*'b,'l propa) message
val unsat   : 'sign -> 'j             -> ('sign,'j*_,unsat) message
val straight: 'sign -> 'j -> 'b       -> ('sign,'j*'b,straight) message
val both    : 'sign -> 'j -> 'b -> 'b -> ('sign,'j*'b,both) message
val either  : 'sign -> 'j -> 'b -> 'b -> ('sign,'j*'b,either) message

(* Printing messages *)

val print_msg_in_fmt: (Format.formatter -> 'j -> unit)
                      -> (Format.formatter -> 'b -> unit)
                      -> Format.formatter -> (_,'j*'b,_)message -> unit
