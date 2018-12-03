module type PreConstraints = sig
  (* The type of a constraint *)
  type t
  val equal : t -> t -> bool
    
  (* A partial comparison
  None if the two elements are not comparable, or follow the same rules as compare:
    a positive integer if a > b
    0 if a == b
    a negative integer if a < b *)
  val partialCompare : t -> t -> int option

  (* Determine if a set of constraint is consistent (satisfiable) or not *)
  val isConsistent : t list -> bool
end



                           

(* Module describing constraints and a total order on them *)
module type Constraints = sig
  (* The type of a constraint *)
  type t
  val equal : t -> t -> bool
    
  (* Comparison over the constraints. Must be a total order *)
  val compare : t -> t -> int

  (* Determine if a set of constraints is consistent (satisfiable) or not *)
  val isConsistent : t list -> bool
end



                        

(* Linearize a partial order into a total order, preserving the type and consistency check of the constraints module *)
module MakeTotal (PC : PreConstraints) : Constraints
  with type t = PC.t


(* Create a module implementing the QuickXplain algorithm
onto the constraints type, order and comparison checker passed a module argument


see Junker, U. QuickXPlain : Preferred Explanations and Relaxations for Over-Constrained Problems, for details *)
module Make (C : Constraints) : sig

  (* Background Constraints -> Relaxable Constraints -> Preferred Conflict (if it exists) *)
  val quickXplain : C.t list -> C.t list -> C.t list option
end
