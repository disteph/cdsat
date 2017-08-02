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
                         
type (_,_) propagated =
  | Unsat    : (_,unsat_l) propagated
  | Straight : 'b -> ('b,straight_l) propagated
                                                 
type (_,_,_) message =
  | Sat   : { assign : 'j; sharing:'tset; myvars:'tset } -> (_,'j*_*'tset,sat) message
  | Propa : 'j * ('b,'l) propagated -> (_,'j*'b*_,'l propa) message

(* Message construction functions *)
                                                                  
val sat     : 'sign -> 'j -> sharing:'tset -> myvars:'tset -> ('sign,'j*_*'tset,sat) message
val propa   : 'sign -> 'j -> ('b,'l) propagated -> ('sign,'j*'b*_,'l propa) message
val unsat   : 'sign -> 'j                       -> ('sign,'j*_*_,unsat) message
val straight: 'sign -> 'j -> 'b                 -> ('sign,'j*'b*_,straight) message

(* Printing messages *)

val print_msg_in_fmt: (Format.formatter -> 'j -> unit)
                      -> (Format.formatter -> 'b -> unit)
                      -> (Format.formatter -> 'tset -> unit)
                      -> Format.formatter -> (_,'j*'b*'tset,_)message -> unit
