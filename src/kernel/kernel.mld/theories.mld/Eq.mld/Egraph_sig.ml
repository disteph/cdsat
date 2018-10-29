open General
open Sums       

open Top
open Sassigns
open Terms
open Values
open Messages

module type Valuation = sig
  module Revealed : Patricia.Map.S_NH with type keys   = Term.t
                                       and type values = CValue.t * (Assign.t*int) Lazy.t
                                       and type common = int
                                       and type branching = int
  type t
  val reveal : t -> Revealed.t
end


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

  type valuation

  val watchfind : 'a Values.Key.t -> howmany:int -> TSet.t -> t -> t * valuation watch
  
end
