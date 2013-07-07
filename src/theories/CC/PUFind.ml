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
