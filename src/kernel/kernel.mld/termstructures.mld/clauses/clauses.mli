open Top.Specs

open General

                      
(* Type of maps from rational variables to rational coefficients
   infos is the cardinal of the set *)
type 'data varmap = ('data termF, bool, int, int, int*[`NoHCons]) Patricia.poly

     
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)

(* Representation of a clause, together with its negation: None is
   used to represent the trivially true clause, i.e. the negation of
   the empty clause. Apart from this case, a clause and its negation
   are usually Some a, Some b, with one of a or b being a singleton
   and the other one being non-empty. *)

type ('data,'tset) t = private
  { asclause : 'data varmap option; (* None if trivially true *)
    ascube   : 'data varmap option; (* None if trivially false *)
    freevar  : 'tset }

module TS : Termstructure.Type with type ('d,'ts) t = ('d,'ts) t
