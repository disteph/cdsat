open General.Sums       

(* The is the interface for the raw implementation of the egraph, i.e. the union-find structure.
     A pointed component is an EGraph component of a specific node;
     it is pointed because it remembers which node it is the component of. *)
       
module type S = sig
  type node
  type edge
  type info
  type termmap
  type _ egraph
  type t = EGraph : _ egraph -> t

  val extract : t -> termmap

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

module type NodeMap = sig
  type t
  type node
  type value
  val empty : t
  val mem   : node -> t -> bool
  val find  : node -> t -> value
  val add   : node -> value -> t -> t
end

module type Parameters = sig
  module Node : sig
    type t [@@deriving eq,ord,hash,show]
  end
  type edge [@@deriving show]
  type info
  (* Parent element in e-graph, compressing paths.
     Root representative is mapped to the component information, to which we add:
     the id of the component and its size *)
  module NodeMapCompress : NodeMap with type node := Node.t
                                    and type value := (Node.t,int*int*info) sum
  (* Parent element, without path compression. Root is mapped to None.
     Used to produce explanations. *)
  module NodeMapProof : NodeMap with type node := Node.t
                                 and type value := (Node.t*edge) option

end

