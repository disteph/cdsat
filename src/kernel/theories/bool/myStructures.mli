open Top.Specs

open Prop.Literals

open General
open Patricia
open Patricia_interfaces


module I : Intern with type keys = LitF.t
                   and type common = int
                   and type branching = int
                      
(* LSet = Sets of literals, patricia tries implementation (hconsed) *)

module LSet : sig

  include PATSetType
          with type e = LitF.t
           and type infos = int
           and type ('v, 'i) param =
                      (LitF.t, 'v, I.common, I.branching, 'i) General.Patricia.poly
  val next : t -> e*t
end
     
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)

type data = private {
    aslit    : LitF.t;
    asclause : LSet.t option;
    nasclause: LSet.t option
  }
                                                           
(* Representation of a clause, together with its negation: None is
     used to represent the trivially true clause, i.e. the negation of
     the empty clause. Apart from this case, a clause and its negation
     are usually Some a, Some b, with one of a or b being a singleton
     and the other one being non-empty. *)

module ThDS : DataType with type t = data
