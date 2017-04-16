open Top
open Messages
open Specs

open General
open Patricia
open Patricia_interfaces
open SetConstructions

open Termstructures
open Literals
open Clauses
       
module Make(DS: sig 
                include GTheoryDSType
                val proj: Term.datatype -> Clauses.TS.t
              end) : sig

  open DS

  module LMap : PATMapType with type keys = LitF.t
                           and  type values = Term.t
                           and  type infos  = unit
                           and  type ('v,'i) param = (LitF.t,'v,I.common,I.branching,'i) poly
                           and  type t = (LitF.t,Term.t,I.common,I.branching,unit) poly
                                       
  val litAsTerm : LitF.t -> Term.t

  module Model : sig
    type t
    val reveal : t -> LMap.t * LMap.t
    val empty : t
    val add : LitF.t -> t -> Term.t * t
  end

  module Constraint : sig
    include FromHConsed
    val make : Term.t -> t
    val term : t -> Term.t
    val simpl: t -> (LSet.t*(Model.t list), Term.t option) Sums.sum
    val verysimpl: t -> (LSet.t, Term.t option) Sums.sum
    val simplify : Model.t->t->t
    val print_in_fmt : Format.formatter -> t -> unit
  end

  val clear : unit -> unit

  type uc_clause = {
      term : Term.t;
      info : (LitF.t option * Model.t list, TSet.t) Sums.sum
    }

  module T2Clause : Map.S with type key = Term.t

  type straight =
    (unit, TSet.t, Messages.straight) Top.Messages.message

  val explain :
    Term.t -> LitF.t option -> Model.t list -> DS.TSet.t

  val formThStraight :
    Term.t
    -> uc_clause T2Clause.t
    -> (Term.t * straight * uc_clause T2Clause.t) option

  val explain_relevant :
    TSet.t
    -> uc_clause T2Clause.t
    -> straight list * uc_clause T2Clause.t

end
