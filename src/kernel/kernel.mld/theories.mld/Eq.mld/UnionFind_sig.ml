open General
open Monads
open Sums

(* The is the interface for the raw implementation of the egraph, i.e. the union-find structure.
     A pointed component is an EGraph component of a specific node;
     it is pointed because it remembers which node it is the component of. *)
       
module type S = sig

  module EMonad : Monad
  module Let_syntax : Let_syntax with type 'a t := 'a EMonad.t
  type node
  type edge
  type info
  type termmap

  type t

  val extract : termmap EMonad.t

  val init  : t
  val force : 'a EMonad.t -> t -> 'a*t

  val get_info  : node -> info EMonad.t

  (* See if two nodes are in the same component *)
  val are_connected : node -> node -> bool EMonad.t

  (* Update info of pointed component *)
  val update : node -> info -> unit EMonad.t

  (* Merge 2 pointed components, adding edge between the 2 points,
     using info for merged component *)
  val merge : node -> node -> info -> edge -> unit EMonad.t

  (* Add new singleton component.
     If it exists, returns the original graph. *)
  val add   : node -> info -> unit EMonad.t

  type path = private { first : node;
                        tail  : (edge*node) list } [@@deriving show]

  val path_tail : path -> path
  val path_cons : (edge*node*node) -> path -> path
  val path_rev_append : path -> path -> path

  (* path t t' eg
     provides path from t to t'.
     t is assumed to belong to component pc, otherwise this function breaks.
  *)
  val get_path : node -> node -> path EMonad.t

  val elazy : 'a EMonad.t -> 'a Lazy.t EMonad.t
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

