open General
open Sums       
open Monads
    
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

  module EMonad : Monad
  module Let_syntax : Let_syntax with type 'a t := 'a EMonad.t

  type info
  type termvalue
         
  type t
  val init  : t
  val force : 'a EMonad.t -> t -> 'a*t

  val eq : Term.t -> (Term.t,Value.t values)sum -> SAssign.t -> level:int ->
    (info*termvalue list) EMonad.t
  val diseq : Term.t -> Term.t -> SAssign.t -> level:int -> unit EMonad.t
  (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
  val ask : ?subscribe:bool -> termvalue -> info EMonad.t

  val nf       : info -> Term.t
  val cval     : info -> CValue.t
  val distinct : info -> CValue.t list EMonad.t

  val watchfind : 'a Values.Key.t -> howmany:int -> TSet.t ->
    unit Valuation.signed watch EMonad.t
  
end
