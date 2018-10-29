open General.Sums       
open Top
open Sassigns
open Messages
open Terms
open Values
    
(* API for plugin. So far, let's pretend it's empty *)

type 'sign output =
  | UNSAT of (('sign, straight) imessage list * ('sign, unsat) imessage)
  | SAT of ('sign, sat) message * 'sign self

and 'sign self = { add   : SAssign.t -> level:int -> 'sign output;

                   share : TSet.t -> 'sign output;

                   watchfind : 'a . 'a Values.Key.t -> howmany:int -> 
                     TSet.t -> Egraph.Valuation.t Egraph.watch * 'sign self;

                   ask : ?subscribe:bool
                     -> (Term.t,Value.t values) sum
                     -> Term.t
                        * CValue.t
                        * (unit -> CValue.t list)
                        * 'sign self }

module type API = sig
  type sign
  val init : sign self
end
