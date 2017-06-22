open General.Sums       
open Top
open Specs
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
    val get   : 'a egraph -> node -> 'a t
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

  (* Provide path between 2 nodes of the same component *)
  val path  : node -> node -> _ egraph -> edge list
end

module type Parameters = sig
  module Node : sig
    type t [@@deriving eq,ord]
  end
  type edge
  type info
end

module type Egraph = sig

  type stop
  exception Conflict of stop

  type term
  type termValue
  type sassign
  type info
  type cval
         
  type t
  val init : t
  val eq : term -> termValue -> sassign -> t -> t*info*(termValue list)
  val diseq : term -> term -> (term*bool) -> t -> t
  (* Ask information about the termvalue,
       possibly subscribe (subscribe=true) or unsubscribe (subscribe=false)
       to notifications when the termvalue sees its combined value affected *)
  val ask : ?subscribe:bool -> termValue -> t -> info*t

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

  type nonrec sassign = (termdata,value) sassign
  type boolassign = termdata termF * bool
  type straight =
    (unit, assign * boolassign, Top.Messages.straight)
      Top.Messages.message
  type stop =
    straight list *
      (unit, assign * boolassign, Top.Messages.unsat)
        Top.Messages.message
        
  module type SlotMachineEq = sig
    type t
    val treated : assign
    val add     : sassign -> t
  end

  type outputEq =
    | UNSAT of stop
    | SAT of
        (sign, assign * boolassign, sat) message
        * (module SlotMachineEq with type t = outputEq)
            
  val init : (module SlotMachineEq with type t = outputEq)

end
