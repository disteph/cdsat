open General.Sums       
open Top
open Specs
open Sassigns
open Messages
       
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

  type term
  type value
  type info
  type cval
         
  type t
  val init : t
  val eq : term -> (term,value values)sum -> ((term,value)bassign,(term,value)sassign)sum
           -> t -> t*info*((term,value values)sum list)
  val diseq : term -> term -> (term,value)bassign -> t -> t
  (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
  val ask : ?subscribe:bool -> (term,value values)sum -> t -> info*t

  val nf : info -> term
  val cval : info -> cval
  val distinct : t -> info -> cval list
                                                        
end

(* API for plugin. So far, let's pretend it's empty *)

module type API = sig

  type sign
  type termdata
  type value
  type assign
  type cval

  type nonrec sassign = (termdata termF,value) sassign
  type nonrec bassign = (termdata termF,value) bassign
  type straight = (sign, assign * bassign, Top.Messages.straight) message
        
  type output =
    | UNSAT of (straight list
               * (sign, assign * bassign, unsat) message)
    | SAT of (sign, assign * bassign, sat) message
             * self

   and self = {
       treated : assign;
       add     : sassign -> output;
       ask     : ?subscribe:bool
                 -> (termdata termF,value values) sum
                 -> (termdata termF)
                    * cval
                    * (unit -> cval list)
                    * self
     }
                
  val init : self

end
