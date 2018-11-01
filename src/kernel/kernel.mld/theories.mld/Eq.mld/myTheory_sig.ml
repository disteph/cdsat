open General.Sums       
open Top
open Sassigns
open Messages
open Terms
open Values

(* API for plugin. *)

type 'a watch = 'a Egraph.watch = { fixed     : 'a;
                                    unknown   : TSet.t;
                                    watchable : Term.t list }
                 
type 'sign output =
  | UNSAT of (('sign, straight) imessage list * ('sign, unsat) imessage)
  | SAT of ('sign, sat) message * Term.t list * 'sign self

and 'sign self = { add   : SAssign.t -> level:int -> 'sign output;

                   share : TSet.t -> 'sign output;

                   watchfind : 'a . 'a Values.Key.t -> howmany:int -> 
                     TSet.t -> 'sign Valuation.signed watch * 'sign self;

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
