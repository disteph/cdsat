open General.Sums       
open Top
open Sassigns
open Messages
open Terms
open Values
    
(* The is the interface for the raw implementation of the egraph, i.e. the union-find structure.
     A pointed component is an EGraph component of a specific node;
     it is pointed because it remembers which node it is the component of. *)

module type RawEgraph = sig
  type node
  type edge
  type info
  type _ egraph
  type t = EGraph : _ egraph -> t

  module PC : sig
    type _ t
    val equal : 'a t -> 'a t -> bool
    (* Getting the pointed component of a node.
       Fails if node does not exist. *)
    val get   : 'a egraph -> node -> 'a egraph * 'a t
    (* Get info of pointed component *)
    val get_info  : _ t -> info
  end

  val init  : t

  (* Update info of pointed component *)
  val update : 'a PC.t -> info -> 'a egraph -> t

  (* Merge 2 pointed components, adding edge between the 2 points,
     using info for merged component *)
  val merge : 'a PC.t -> 'a PC.t -> info -> edge -> 'a egraph -> t

  (* Add new singleton component.
     If it exists, returns the original graph. *)
  val add   : node -> info -> _ egraph -> t

  (* path t pc eg
     provides path from t' to t, where pc is "the component of t' "
     (i.e. pc has been obtained by a call of PC.get on t').
     t is assumed to belong to component pc, otherwise this function breaks.
   *)
  val path  : node -> 'a PC.t -> 'a egraph -> edge list
end

module type Parameters = sig
  module Node : sig
    type t [@@deriving eq,ord,hash,show]
  end
  type edge [@@deriving show]
  type info
end

module type Egraph = sig

  type stop
  exception Conflict of stop

  type info
         
  type t
  val init : t
  val eq : Term.t -> (Term.t,Value.t values)sum -> (bassign,SAssign.t)sum
           -> t -> t*info*((Term.t,Value.t values)sum list)
  val diseq : Term.t -> Term.t -> bassign -> t -> t
  (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
  val ask : ?subscribe:bool -> (Term.t,Value.t values)sum -> t -> info*t

  val nf : info -> Term.t
  val cval : info -> CValue.t
  val distinct : t -> info -> CValue.t list
                                                        
end

(* API for plugin. So far, let's pretend it's empty *)

module type API = sig

  type sign

  type output =
    | UNSAT of ((sign, straight) message list * (sign, unsat) message)
    | SAT of (sign, sat) message * self

   and self = { add : SAssign.t -> output;
                share : TSet.t -> output;
                ask : ?subscribe:bool
                      -> (Term.t,Value.t values) sum
                      -> Term.t
                         * CValue.t
                         * (unit -> CValue.t list)
                         * self }
                
  val init : self

end
