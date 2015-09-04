(********************************)
(* Interfaces for CC(X):        *)
(* SolvableTheory and UnionFind *)
(********************************)

open Top
open Basic
open Specs

module type MapImplem = sig
  type e
  type v
  type t
  val find : e -> t -> v
  val empty: t
  val add: e -> v -> t -> t
  val union: t -> t -> t
  val remove: e -> t -> t
  val map : (v -> v) -> t -> t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
end

module type SetImplem = sig
  type e
  type t
  val empty : t
  val mem : e -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val add : e -> t -> t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
end

type 'a input = 
| Eq of Sorts.t*'a*'a
| NEq of Sorts.t*'a*'a
| Congr of 'a*'a

(* signature of the theory X in input for CC(X) *)

module type SolvableTheory = sig

  type t (* terms *)

  type v (* semantic values *)

  (* sets of semantic values *)
  module VSet : SetImplem with type e = v

  (* Maps from semantic values to term sets *)
  module VtoTSet : MapImplem with type e = v

  (* maps from semantic values to semantic values *)
  module VtoV : MapImplem with type e = v and type v = v

  (* compute the sem. value of a term *)
  val make : t -> v

  (* values of maximal uninterpreted terms in the value of a term *)
  val leaves : v -> VSet.t

  (* apply a substitution on a sem. value *)
  val subst : v -> v -> v -> v

  type res = Sol of v*v | Bot | Top

  (* try to find a substitution to unify the two values in input *)
  val solve : v -> v -> res

end




module type PersistentUnionFind = sig
  (* type of the elements of the equivalence classes *)
  type e
  (* type of the labels on the arcs *)
  type d
  (* type of the structure containing the classes *)
  type t
  (* empty structure *)
  val create : t
  (* add a new class for the element *)
  val add : t -> e -> t
  (* add an arc between the 2 elements, with the label and
     the 2 elements become the roots of their class (before adding the arc)
     if the second element isn't in the structure, it is created
     if an arc already exists it isn't replaced *)
  val addLink : t -> e -> e -> d -> t
  (* give the representative *)
  val find : t -> e -> e
  (* merge the classes of the 2 elements *)
  val union : t -> e -> e -> t
  (* give the list of the labels on the path from the element to
     its representative *)
  val path : t -> e -> d list
  (* give the list of the labels on the path from the first element to
     the second element *)
  val pathTo : t -> e -> e -> d list
  (* give the first common ancestor of 2 elements of the same class *)
  val fca : t -> e -> e -> e
  (* clean the structures used for memoisation *)
  val clear : unit -> unit
end
