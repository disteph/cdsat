open Format

open General
open Kernel.Top
open Kernel.Theories.Bitvectors

open QuickXplain

(* Converts a valuation given by BDD.sat or BDD.allsat in a bitvector of type Bv_value.t (constructed by HardCaml)
  valuation: <(bool * var) list> := a list of pair (value, variable) describing an valuation of the variables of a BDD
  RETURNS : <Bv_value.t> := bitvector representation of valuation. Unassigned variable are assigned 0 by default
  *)
let valuation_to_bitvector width valuation =
  let construct_list width pair_list =
    let rec aux plist w_acc l_acc = if w_acc = width then l_acc else match plist with
      | [] -> aux plist (w_acc+1) (0::l_acc)
      | (value, var)::t when var = w_acc -> aux t (w_acc+1) ((if value then 1 else 0)::l_acc)
      | _ -> aux plist (w_acc+1) (0::l_acc)
    in
    aux pair_list 0 []
  in
  Bv_value.constibl (
    construct_list width (List.sort
      (fun (lvalue, lvar) (rvalue, rvar) -> lvar - rvar)
      valuation) (* sorted in increasing order *)
    )

(* Type of a bitvector range representing all the bitvector that still satisfies the list 'history' of conditions *)
type 'a t = {width: int; bdd: BDD.t; history: ('a * BDD.t) list; pick: Bv_value.t}

let pp fmt {width; bdd} = BDD.pp (fun fmt i -> fprintf fmt "%i" i) fmt bdd
let show r = Print.stringOf pp r

(* Default range, containing all bitvectors of length 'width' *)
let init n = {width = n; bdd = BDD.dtrue; history =[]; pick = valuation_to_bitvector n []}



 (* Gives an element  (of type Bv_value.t) in the bitvector range
  {pick} : subset of <'a t> := the bitvector range to choose an element from
  RETURNS : <Bv_value.t> := a bitvector in the range
 *)
let pick {pick} = pick

(* Test if the bitvector is in the bitvector range
  bitvector : <Bv_value.t> := the bitvector to test membership on
  {width; bdd} : subset of <'a t> := the bitvector to test membership of
  RETURNS : bool := true if the bitvectir belongs to the range, false otherwise

*)
let mem bitvector {width; bdd} =
let one = Bv_value.consti 1 1 in
let bitArray = Array.of_list (List.map (fun w -> Bv_value.equal w one) (Bv_value.bits bitvector)) in
  let rec iter_bdd tree flip = match BDD.inspect tree with
    | BDD.False -> if flip then true else false
    | BDD.True -> if flip then false else true
    | BDD.Not t -> iter_bdd t (not flip)
    | BDD.If (left, var, right) -> iter_bdd (if bitArray.(var) then right else left) flip
  in
  iter_bdd bdd false


(* type for an update of the range, distinguishing empty range, singleton range and other range *)
type 'a update =
  | Range of 'a t
  | Singleton of Bv_value.t
  | Empty of ('a * BDD.t) list

(* Update the range to also satisfies a new condition 
  signal : <Signal.t> := the bitvector of length 1 representing the constraints imposed by the condition
  condition : <'a> := the original condition that created the constraints
  old_range : <'a t> := the range to update
  RETURNS : <'a update> := the updated range also satisfying condition, with special cases distinguished
*)
let update signal condition ({width; bdd; history} as old_range) =
  if Signal.width signal <> 1 then failwith "Expected signal of width 1 representing bool formula";
  let rec find_valuation tree unique_acc val_acc = match BDD.inspectb tree with
  | BDD.BFalse -> failwith "Explored un-sat BDD"
  | BDD.BTrue -> ([], false)
  |BDD.BIf (tl, var, tr) -> match BDD.inspectb tl, BDD.inspectb tr with
    | BDD.BFalse, BDD.BTrue -> (true, var)::val_acc, unique_acc
    | BDD.BTrue, BDD.BFalse -> (false, var)::val_acc, unique_acc
    | BDD.BFalse, _ -> find_valuation tr unique_acc ((true, var)::val_acc)
    | _, BDD.BFalse -> find_valuation tl unique_acc ((false, var)::val_acc)
    | _, BDD.BTrue -> (true, var)::val_acc, false
    | BDD.BTrue, _ -> (false, var)::val_acc, false
    | _, _ -> find_valuation tl false ((false, var)::val_acc)
  in
  let new_constraint = Signal.bit 0 signal in
  let new_bdd = BDD.dand bdd new_constraint in
  if BDD.is_false new_bdd then Empty ((condition, new_constraint)::history) else
  if BDD.equal bdd new_bdd then Range old_range else
  let valuation, uniqueness = find_valuation new_bdd true [] in
  let pick = valuation_to_bitvector width valuation in
  if uniqueness
    then Singleton pick
    else Range {width = width; bdd = new_bdd; history = (condition, new_constraint)::history; pick}



let make_explanation_module (type a) (e : a update) = match e with
  | Empty l ->
    (module struct
      type t = a * BDD.t
      let equal (a:t) (b:t) = a==b
      let data = l
      let isConsistent (lst : t list) : bool =
        let rec aux_consistent (lst : t list) (acc : BDD.t) = match lst with
          | [] -> acc
          | (condition, bdd)::t -> aux_consistent t (BDD.dand acc bdd)
        in
        not (BDD.is_false (aux_consistent lst BDD.dtrue))
      
      (* Compares the two conditions. The preferred (smaller) condition is the one
      the most deep down the data list, i.e. the conditions coming from the earliest choices during
      tree searching 
      If either of the two condition is not found, they are considered not comparable *)
      let partialCompare (a : t) (b : t) : int option =
        let rec aux lst a b = match lst with
          |[] -> None, false
          |h::t when equal a h -> Some a, List.exists (equal b) t
          |h::t when equal b h -> Some b, List.exists (equal a) t
          |h::t -> aux t a b
        in match aux data a b with
        |Some ap, true when equal ap a -> Some 1
        |Some bp, true when equal bp b -> Some (-1)
        |_, _ -> None
    end : QuickXplain.PreConstraints with type t = a*BDD.t)
  | _ -> failwith "Cannot explain non-empty updated range"


(* Find a preferred conflict on an update of type Empty by using the QuickXplain algorithm
  That is, it finds a subset of the ('a * BDD.t) list of the Empty update that is still inconsistent
  and by favoring the earliest conditions.
  e : <Empty of ('a * BDD.t) list> of type update := the empty range to explain
  RETURNS <('a * BDD.t) list> a subset of the list of e that still makes the range empty, but
        with preferred constraints only
        
  NOTE: the subset found is valid only with the full valuation
  that gave the constraints that rendered the range empty (through update).
  That is because the BDDs of the constraints that were given to "update" are saved within the "history" member of
  the type 'a t to improve computation, but this supposes that the constraints remain the same and therefor that
  the valuation is complete*)
let find_explanation (type a) (e : a update) = match e with
  | Empty l -> let (module Arg)=make_explanation_module e in
               let module M = QuickXplain.Make (QuickXplain.MakeTotal (Arg)) in
        M.quickXplain [] l
  | _ -> failwith "Cannot explain non-empty updated range"
