open General
open Sums       

open Top
open Sassigns
open Terms
open Values
open Messages

type stop = (unit,straight) imessage list * (unit,unsat) imessage
exception Conflict of stop

type 'valuation watch = { fixed     : 'valuation;
                          unknown   : TSet.t;
                          watchable : Term.t list }

module type S = sig

  type info
         
  type t
  val init : t
  val eq : Term.t -> (Term.t,Value.t values)sum -> SAssign.t -> level:int ->
    t -> t*info*((Term.t,Value.t values)sum list)
  val diseq : Term.t -> Term.t -> SAssign.t -> level:int -> t -> t
  (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
  val ask : ?subscribe:bool -> (Term.t,Value.t values)sum -> t -> info*t

  val nf       : info -> Term.t
  val cval     : info -> CValue.t
  val distinct : t -> info -> CValue.t list

  val watchfind : 'a Values.Key.t -> howmany:int -> TSet.t -> t -> t * unit Valuation.signed watch
  
end
