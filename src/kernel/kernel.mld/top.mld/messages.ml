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
  | Straight : 'b -> ('b,straight_l) propagated
  | Both     : 'b * 'b -> ('b,both_l) propagated
  | Either   : 'b * 'b -> ('b,either_l) propagated
                                                 
type (_,_,_) message =
  | Sat   : 'j -> (_,'j*_,sat) message
  | Propa : 'j * ('b,'l) propagated -> (_,'j*'b,'l propa) message

(* Message construction functions *)
                                                                  
let sat _ justif              = Sat justif
let propa _ justif p          = Propa(justif,p)
let unsat _ justif            = Propa(justif,Unsat)
let straight _ justif b       = Propa(justif,Straight b)
let both   _ justif b1 b2 = Propa(justif,Both(b1,b2))
let either _ justif b1 b2 = Propa(justif,Either(b1,b2))

(* Printing messages *)

let print_msg_in_fmt_latex j_pp b_pp fmt (type a): (_,_,a)message -> unit = function
  | Sat justif
    -> fprintf fmt "Sat(%a)" j_pp justif
  | Propa(justif,Unsat)
    -> fprintf fmt "%a\\vdash\\bot" j_pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a\\vdash %a" j_pp justif b_pp b
  | Propa(justif,Both(b1,b2))
    -> fprintf fmt "%a\\vdash (%a)\\bigwedge (%a)" j_pp justif b_pp b1 b_pp b2
  | Propa(justif,Either(b1,b2))
    -> fprintf fmt "%a\\vdash (%a)\\bigvee (%a)" j_pp justif b_pp b1 b_pp b2

let print_msg_in_fmt_utf8 j_pp b_pp fmt (type a): (_,_,a)message -> unit = function
  | Sat justif
    -> fprintf fmt "Sat(%a)" j_pp justif
  | Propa(justif,Unsat)
    -> fprintf fmt "%a ⊢ ⊥" j_pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a ⊢ %a" j_pp justif b_pp b
  | Propa(justif,Both(b1,b2))
    -> fprintf fmt "%a ⊢ (%a) ⋁ (%a)" j_pp justif b_pp b1 b_pp b2
  | Propa(justif,Either(b1,b2))
    -> fprintf fmt "%a ⊢ (%a) ⋀ (%a)" j_pp justif b_pp b1 b_pp b2

let print_msg_in_fmt j_pp b_pp fmt = match !Dump.display with
  | Dump.Latex -> print_msg_in_fmt_latex j_pp b_pp fmt
  | _ -> print_msg_in_fmt_utf8 j_pp b_pp fmt
