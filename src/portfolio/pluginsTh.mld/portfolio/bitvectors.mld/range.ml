open Format

open General
open Kernel.Top
open Kernel.Theories.Bitvectors
       
(* type 'a t = BDD.t * 'a list *)
type 'a t = {width: int; bdd: BDD.t; history: 'a list}


let pp fmt {width; bdd} = BDD.pp (fun fmt i -> fprintf fmt "%i" i) fmt bdd
let show r = Print.stringOf pp r
  
let init n = {width = n; bdd = BDD.dtrue; history =[]}

(* pick  : range -> valeur ;;; sert a choisir une valeur dans un range *)
(* The required return type is MyTheory.V.t, which must be constructed using MyTheroy.V.consti
 * This function requires first the total number of variables, then an integer whose bits assign true/false to variables
 * The number of variables is not available and is therefor guessed to be the maximum variable index in the BDD, extracted in the max_var argument
 * The integer ifs constructed by interating on the list of the assigned variables in the solution proposed by BDD.sat
 *)
 (* TODO: use list MyTheory.V.constibl , plus robuste *)
let pick {width; bdd} =
  let construct_list width pair_list =
    let aux plist w_acc l_acc = if w_acc = width then l_acc else match plist with
      | [] -> aux plist (w_acc+1) (0::l_acc)
      | (value, var)::t when var = w_acc -> aux t (w_acc+1) ((if value then 1 else 0)::l_acc)
      | _ -> aux plist (w_acc+1) (0::l_acc)
    in
    aux pair_list 0 []
  in
  match BDD.sat bdd with (* Return <(bool * var) list option> of the pair (value, variable_id) *)
    |None -> failwith "EMPTY"
    |Some(alist) -> MyTheory.V.constibl (construct_list
      width
      (List.sort (fun (lvalue, lvar) (rvalue, rvar) -> lvar - rvar) alist) (* sorted in increasing order*)
    )

(* test si valeur est dans l'ensemble : membership *)
let mem assignement {width; bdd} =
let one = MyTheory.V.consti 1 1 in
let bitArray = Array.of_list (List.map (fun w -> w = one) (MyTheory.V.bits assignement)) in
  let iter_bdd tree flip = match BDD.inspectb tree with
    | BDD.False -> if flip then BDD.True else BDD.False
    | BDD.True -> if flip then BDD.False else BDD.True
    | Not t -> iter_bdd t (!flip)
    | If (left, var, right) -> iter_bdd (if bitArray.(var) then right else left) flip
  in
  iter_bdd bdd false

type 'a update =
  | Range of 'a t
  | Singleton of MyTheory.V.t
  | Empty of 'a list

(* Voir .mli, on update quand une contrainte deviens unitaire en une variable 
La contrainte est un bool dépdendant des bits de la variable (premier ARG)
La contrainte est le arg 2
prend un range
founri le range updaté
*)
(* TODO: VERIFY *)
(* TODO: parcourir new_bdd en profondeur en vérifiant que exactement 1 des fils est BFalse, utilisez API standard*)
let update signal formula ({width; bdd; history} as old_range) =
  let new_bdd = BBD.dand bdd signal.(0) in
  if (BDD.equal new_bdd bdd) then
    match BDD.allsat new_bdd with
    | a::b::t -> Range {width = width; bdd = new_bdd; history = formula::history}
    | a::[] -> Singleton a (* TODO: convertir, cf au dessus *)
    | [] -> Empty formula::history
  else old_range
