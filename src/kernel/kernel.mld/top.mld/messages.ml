(*****************)
(* Message types *)
(*****************)

open Format

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
  | Sat   : { assign : 'j; sharing:'tset; myvars:'tset Lazy.t} -> (_,'j*_*'tset,sat) message
  | Propa : 'j * ('b,'l) propagated -> (_,'j*'b*_,'l propa) message

(* Message construction functions *)

let sat _ assign ~sharing ~myvars = Sat{ assign; sharing; myvars }
let propa _ justif p     = Propa(justif,p)
let unsat _ justif       = Propa(justif,Unsat)
let straight _ justif b  = Propa(justif,Straight b)

(* Printing messages *)

let print_msg_in_fmt_latex j_pp b_pp tset_pp fmt (type a): (_,_,a)message -> unit = function
  | Sat { assign; sharing; myvars }
    -> fprintf fmt "Sat(%a) sharing %a (my vars are %a)"
         j_pp assign tset_pp sharing tset_pp (Lazy.force myvars)
  | Propa(justif,Unsat)
    -> fprintf fmt "%a\\vdash\\bot" j_pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a\\vdash %a" j_pp justif b_pp b

let print_msg_in_fmt_utf8 j_pp b_pp tset_pp fmt (type a): (_,_,a)message -> unit = function
  | Sat { assign; sharing; myvars }
    -> fprintf fmt "Sat(%a) sharing %a (my vars are %a)"
         j_pp assign tset_pp sharing tset_pp (Lazy.force myvars)
  | Propa(justif,Unsat)
    -> fprintf fmt "%a ⊢ ⊥" j_pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a ⊢ %a" j_pp justif b_pp b

let print_msg_in_fmt j_pp b_pp fmt = match !Dump.display with
  | Dump.Latex -> print_msg_in_fmt_latex j_pp b_pp fmt
  | _ -> print_msg_in_fmt_utf8 j_pp b_pp fmt

