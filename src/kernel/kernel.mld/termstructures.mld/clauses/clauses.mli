open Top.Terms

open General

module VarMap : sig
  include Patricia.Map.S_NH
    with type keys = Term.t
     and type values = bool
     and type common = int
     and type branching = int
     and type infos = int (* cardinal *)
  val next : t -> Term.t * bool * t
  val pp : t Format.printer
end
     
(* Representation of terms for boolean reasoning, knowing about
   disjunction, negation, true and false *)

(* Representation of a clause, together with its negation: None is
   used to represent the trivially true clause, i.e. the negation of
   the empty clause. Apart from this case, a clause and its negation
   are usually Some a, Some b, with one of a or b being a singleton
   and the other one being non-empty. *)

type nature = private Var | NVar | Other

type t = private
  { asclause : VarMap.t option; (* None if trivially true *)
    ascube   : VarMap.t option; (* None if trivially false *)
    freevar  : TSet.t;
    nature   : nature }
[@@deriving fields]

val key : t Key.t
