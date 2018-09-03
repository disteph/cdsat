(*****************)
(* Message types *)
(*****************)

open Format
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

type _ propagated =
  | Unsat    : unsat_l propagated
  | Straight : bassign -> straight_l propagated
                                                 
type (_,_) message =
  | Sat   : { assign : Assign.t; sharing: TSet.t; myvars: TSet.t Lazy.t} -> (_,sat) message
  | Propa : Assign.t * 'l propagated -> (_,'l propa) message

(* Message construction functions *)

let sat _ assign ~sharing ~myvars = Sat{ assign; sharing; myvars }
let propa _ justif p     = Propa(justif,p)
let unsat _ justif       = Propa(justif,Unsat)
let straight _ justif b  = Propa(justif,Straight b)

(* Printing messages *)

let print_msg_in_fmt_latex fmt (type a): (_,a)message -> unit = function
  | Sat { assign; sharing; myvars }
    -> fprintf fmt "Sat(%a) sharing %a (my vars are %a)"
         Assign.pp assign TSet.pp sharing TSet.pp (Lazy.force myvars)
  | Propa(justif,Unsat)
    -> fprintf fmt "%a\\vdash\\bot" Assign.pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a\\vdash %a" Assign.pp justif pp_bassign b

let print_msg_in_fmt_utf8 fmt (type a): (_,a)message -> unit = function
  | Sat { assign; sharing; myvars }
    -> fprintf fmt "Sat(%a) sharing %a (my vars are %a)"
         Assign.pp assign TSet.pp sharing TSet.pp (Lazy.force myvars)
  | Propa(justif,Unsat)
    -> fprintf fmt "%a ⊢ ⊥" Assign.pp justif
  | Propa(justif,Straight b)
    -> fprintf fmt "%a ⊢ %a" Assign.pp justif pp_bassign b

let print_msg_in_fmt fmt = match !Dump.display with
  | Dump.Latex -> print_msg_in_fmt_latex fmt
  | _ -> print_msg_in_fmt_utf8 fmt

