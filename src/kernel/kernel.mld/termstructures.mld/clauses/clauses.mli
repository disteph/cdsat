open Top.Specs

open Literals

open General
open Patricia
open Patricia_interfaces

module I : Intern with type keys = LitF.t
                   and type common = int
                   and type branching = int
                      
(* LSet = Sets of literals, patricia tries implementation (hconsed)
   infos is the cardinal of the set *)

module LSet : sig
  include PatSet
          with type e = LitF.t
           and type infos = int
           and type ('v, 'i) param =
                      (LitF.t, 'v, I.common, I.branching, 'i) poly
  val next : t -> e*t
end
     
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)

(* Representation of a clause, together with its negation: None is
   used to represent the trivially true clause, i.e. the negation of
   the empty clause. Apart from this case, a clause and its negation
   are usually Some a, Some b, with one of a or b being a singleton
   and the other one being non-empty. *)

type t' = private { asclause : LSet.t option; (* None if trivially true *)
                   ascube   : LSet.t option; (* None if trivially false *)
                   freevar  : IntSortSet.t }

module TS : Termstructure.Type with type (_,_) t = t'
