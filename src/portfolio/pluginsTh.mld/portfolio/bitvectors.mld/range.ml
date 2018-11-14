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
  (* Tail-recursive fast exponent helper thanks to https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml *)
  (*
  let pow base exponent =
    if exponent < 0 then invalid_arg "exponent can not be negative" else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
    aux 1 base exponent
  in
  match BDD.sat bdd with
    |None -> failwith "EMPTY"
    |Some(alist) -> begin
        MyTheory.V.consti width (List.fold_left 
          (fun base (bl, i) -> if bl then base + (pow 2 i) else base) 0 alist)
        end
  *)
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
let mem assignement {} = "List.map V.to_int (MyTheory.V.bits a)" failwith "TODO"

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
let update _ _ _ = failwith "TODO"
