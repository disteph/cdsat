(****************************************************************)
(**     Polymorphic implementation of QuickXplain in OCaml     **)
(****************************************************************)
(**.ml file**)




(* Module describing constraints and a partial order on them *)
module type PreConstraints = sig
  (* The type of a constraint *)
  type t

  (* A partial comparison
  None if the two elements are not comparable, or follow the same rules as compare:
    a positive integer if a > b
    0 if a == b
    a negative integer if a < b *)
  val equal : t -> t -> bool
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






module MakeTotal (PC : PreConstraints) = struct
  type t = PC.t
  let equal = PC.equal
  let isConsistent = PC.isConsistent

  let reprs = ref []
  let isSome o = match o with |None -> false |Some _ -> true
  let rec hasRepr l a = match l with
    |[] -> false
    |h::t -> if isSome (PC.partialCompare a h) then true else hasRepr t a
  
  let find_reprs (a : t) (b : t) =
    let rec aux_find l a b = match l with
      | [] -> None, false
      | h::t when isSome (PC.partialCompare h a) -> Some a, (hasRepr t b)
      | h::t when isSome (PC.partialCompare h b) -> Some b, (hasRepr t a)
      | h::t -> aux_find t a b
    in
    aux_find !reprs a b
  let compare (a : t) (b : t) : int = match PC.partialCompare a b with
    | Some i -> i
    | None   -> (match find_reprs a b with
      |Some ap, true when PC.equal ap a -> (-1)
      |Some bp, true when PC.equal bp b -> 1
      |Some ap, false when PC.equal ap a -> reprs := b::!reprs; 1
      |Some bp, false when PC.equal bp b -> reprs := a::!reprs; -1
      |None, _ -> reprs := a::b::!reprs; -1
      |Some _, _ -> failwith "INVALID REPR")
end






(* Internal module for ordering list in increasing order *)
module OrderedListSet (C : Constraints) = struct
  type set = C.t list
  let empty = []
  let toSet (a : C.t list) = List.sort C.compare a
  let is_empty (s : set) = match s with |[] -> true |_ -> false
  let union (a : set) (b : set) : set =
    let rec auxUnion _a _b _c = match _a, _b with
      |    [],     [] -> _c
      |     _,     [] -> _a @ _c
      |    [],      _ -> _b @ _c
      |ha::ta, hb::tb when (C.compare ha hb) <= 0 -> auxUnion _a tb (hb::_c)
      |ha::ta, hb::tb -> auxUnion ta _b (ha::_c)
    in
    auxUnion a b []
  let cardinal (s : set) = List.length s

  (*Our divide-and-conquer tactic: tells us where to split.*)
  let split n = n/2

  (*halves returns two halves of c, separated according to our divide-and-conquer tactic split.*)
  let halves (a : set) : set * set =
    let rec auxHalves a acc num = match a, num with
      | _ , 0   -> a, List.rev acc
      | [], _   -> a, List.rev acc
      | h::t, k -> auxHalves t (h::acc) (k-1)
    in
    auxHalves a [] (split (List.length a))
end





                                        
module Make (C : Constraints) = struct

  module Set = OrderedListSet(C)

  type set = Set.set

  (*quickXplain implements the algorithm quickXplain with order E.order.*)
  let _quickXplain b c =
    let rec auxQuickXplain b d c =
      if (not (Set.is_empty d)) && (not (C.isConsistent b)) then Set.empty
      else if (Set.cardinal c) = 1 then c
      else
        let c1,c2 = Set.halves c in
        let d2 = auxQuickXplain (Set.union b c1) c1 c2 in
        let d1 = auxQuickXplain (Set.union b d2) d2 c1 in
        Set.union d1 d2
    in
    if C.isConsistent (Set.union b c) then None
    else if (Set.is_empty c) then Some c
    else
      Some (auxQuickXplain b b c)

  let quickXplain (b : C.t list) (c : C.t list) : C.t list option = _quickXplain (Set.toSet b) (Set.toSet c)
  
end

