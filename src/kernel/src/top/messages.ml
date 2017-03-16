(*****************)
(* Message types *)
(*****************)

open Format

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
  | Propa : 'tset * ('tset,'a) propagated -> (_,'tset,'a propa) message

(* Message construction functions *)
                                                                  
let sat _ tset              = Sat tset
let propa _ tset p          = Propa(tset,p)
let unsat _ tset            = Propa(tset,Unsat)
let straight _ tset newtset = Propa(tset,Straight newtset)
let both   _ tset new1 new2 = Propa(tset,Both(new1,new2))
let either _ tset new1 new2 = Propa(tset,Either(new1,new2))

(* Printing messages *)

let print_msg_in_fmt_latex tsetprint fmt (type a): (_,_,a)message -> unit = function
  | Sat tset
    -> fprintf fmt "Sat(%a)" tsetprint tset
  | Propa(tset,Unsat)
    -> fprintf fmt "%a\\vdash\\bot" tsetprint tset
  | Propa(oldset,Straight newset)
    -> fprintf fmt "%a\\vdash %a" tsetprint oldset tsetprint newset
  | Propa(oldset,Both(newset1,newset2))
    -> fprintf fmt "%a\\vdash (%a)\\bigwedge (%a)" tsetprint oldset tsetprint newset1 tsetprint newset2
  | Propa(oldset,Either(newset1,newset2))
    -> fprintf fmt "%a\\vdash (%a)\\bigvee (%a)" tsetprint oldset tsetprint newset1 tsetprint newset2

let print_msg_in_fmt_utf8 tsetprint fmt (type a): (_,_,a)message -> unit = function
  | Sat tset
    -> fprintf fmt "Sat(%a)" tsetprint tset
  | Propa(tset,Unsat)
    -> fprintf fmt "%a ⊢ ⊥" tsetprint tset
  | Propa(oldset,Straight newset)
    -> fprintf fmt "%a ⊢ %a" tsetprint oldset tsetprint newset
  | Propa(oldset,Both(newset1,newset2))
    -> fprintf fmt "%a ⊢ (%a) ⋁ (%a)" tsetprint oldset tsetprint newset1 tsetprint newset2
  | Propa(oldset,Either(newset1,newset2))
    -> fprintf fmt "%a ⊢ (%a) ⋀ (%a)" tsetprint oldset tsetprint newset1 tsetprint newset2

let print_msg_in_fmt tsetprint fmt = match !Dump.display with
  | Dump.Latex -> print_msg_in_fmt_latex tsetprint fmt
  | _ -> print_msg_in_fmt_utf8 tsetprint fmt
