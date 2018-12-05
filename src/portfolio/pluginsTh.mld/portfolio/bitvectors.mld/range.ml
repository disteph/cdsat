open Format

open General
open Kernel.Top
open Kernel.Theories.Bitvectors

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

(* type 'a t = BDD.t * 'a list *)
(* Type of a bitvector range representing all the bitvector that still satisfies the list 'history' of conditions *)
type 'a t = {width: int; bdd: BDD.t; history: 'a list; pick: Bv_value.t}


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
  | Empty of 'a list

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
  let new_bdd = BDD.dand bdd (Signal.bit 0 signal) in
  if BDD.is_false new_bdd then Empty (condition::history) else
  if BDD.equal bdd new_bdd then Range old_range else
  let valuation, uniqueness = find_valuation new_bdd true [] in
  let pick = valuation_to_bitvector width valuation in
  if uniqueness
    then Singleton pick
    else Range {width = width; bdd = new_bdd; history = condition::history; pick}
