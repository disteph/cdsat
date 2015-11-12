include Num
include Hashtbl

open Num

type var = string
type value = num

(* Represents an inequation *)
type equation = {coeffs : (var, value) Hashtbl.t;  (* Coeffs *)
                 sup : value;                      (* Sup value *)
                 isStrict : bool;                  (* Is the inequality <= ? *)
                 nVar : int;                       (* Number of active variables *)
                 previous : equation list          (* equation whose linear combination allowed to find this one *)
                }


(* Pretty print an equation *)
let print eq =
  Hashtbl.iter (fun k v ->
    if (not(v =/ num_of_int 0)) then
      begin
      if (v =/ num_of_int 1) then
          Printf.printf "%s + " k
        else begin
          if (v =/ num_of_int (-1)) then
            Printf.printf "-%s + " k
          else
            Printf.printf "%s%s + " (string_of_num v) k;
        end;
      end;
    )
    eq.coeffs;
  Printf.printf (if eq.isStrict then " < %s" else " <= %s") (string_of_num eq.sup)

(* Pretty print a list of equations *)
let rec print_eqs eqs =
  match eqs with
  | [] -> ()
  | [eq] -> print eq; print_string "\n"
  | (t::q) -> print t; print_string " /\\ "; print_eqs q


(* Creates an equation from its subparts *)
let create coeffs sup isStrict previous =
  let count _ v acc =
    if v <>/ num_of_int 0 then acc+1 else acc
  in
  let n = Hashtbl.fold count coeffs 0 in
  {coeffs = coeffs; sup = sup; isStrict = isStrict; nVar = n; previous = previous}

(* Create an equation from a list of coeffs *)
let createFromList coeffs sup isStrict previous =
  let table = Hashtbl.create (List.length coeffs) in
  let f (k,v) =
    Hashtbl.replace table k v
  in
  List.iter f coeffs;
  create table sup isStrict previous

(* Return the coeff associated to the varibale var *)
let getCoeff eq var =
  try Hashtbl.find eq.coeffs var with Not_found -> num_of_int 0

(* Return the upper bound of the equation *)
let getSup eq =
  eq.sup

(* Return if < or <= *)
let isStrict eq =
  eq.isStrict

(* TODO should we directly implement this while constructing the equations ?*)
let getPrevious eq =
  match eq.previous with
  | [] -> [eq]
  | _ -> eq.previous

(* Add equations to previous equations *)
let addDependance eq ll =
  {coeffs=eq.coeffs; sup=eq.sup; isStrict=eq.isStrict; nVar=eq.nVar; previous=eq.previous@ll}

(* Change equations to previous equations *)
let setDependance eq ll =
  {coeffs=eq.coeffs; sup=eq.sup; isStrict=eq.isStrict; nVar=eq.nVar; previous=ll}

(* goes through a list and keeps unique elements *)
let uniq lst =
    let unique_set = Hashtbl.create (List.length lst) in
    List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []


let getPreviousEqs eqs =
  uniq (List.fold_left (fun l eq -> (getPrevious eq)@l) [] eqs)


(* Affect a variable in the inequation. It has to built a new
inequation, even if the variable is not present, for compatibility
reasons with our algorithm (the inequations contain themselves all
the "state stack" stuff)*)
let affectVar eq var value =
    try (
      let c = Hashtbl.find eq.coeffs var in
      let newCoeffs = Hashtbl.copy eq.coeffs in
      Hashtbl.remove newCoeffs var;
      {coeffs   = newCoeffs;
       sup      = eq.sup -/ (c */ value);
       isStrict = eq.isStrict;
       nVar     = eq.nVar - 1;
       previous = [eq]}
    ) with Not_found ->
      {coeffs   = eq.coeffs;
       sup      = eq.sup;
       isStrict = eq.isStrict;
       nVar     = eq.nVar;
       previous = [eq]}


(* Return true if and only if the eqation is an atomic constraint*)
let isAtomic eq =
  eq.nVar == 1

(* Return true if and only if the eqation is a trivial inequation *)
let isTrivial eq =
  let count _ v acc =
    if v <>/ Num.num_of_int 0 then acc+1 else acc
  in
  (Hashtbl.fold count eq.coeffs 0) == 0
(*eq.nVar == 0*)

(* Give an active variable if it exists or raise a Not_found exception *)
exception Var_found of var

(* return an active variable, eg a variable of the equation whose coeff is non-nul*)
(* ensure we are not looking to an ancient variable masked by add (should not be possible)*)
let getActiveVar eq =
  let f k v =
    if v <>/ (Num.num_of_int 0) && v =/ Hashtbl.find eq.coeffs k then raise (Var_found k)
  in try Hashtbl.iter f eq.coeffs; raise Not_found with Var_found k -> k

(* get an active variable that is not the one in argument *)
let getAnotherActiveVar eq variable =
  let f k v =
    if k <> variable && v <>/ (num_of_int 0) && v =/ Hashtbl.find eq.coeffs k then raise (Var_found k)
  in try Hashtbl.iter f eq.coeffs; raise Not_found with Var_found k -> k

(* creates a new inequation by multiplying all coefficients by
   a positive or negative value.
   /!\ It does NOT implement the behavior of changing the
   inequation's sense. *)
let multiply eq value =
  let newCoeffs = Hashtbl.create 10 in
  let f k v = Hashtbl.replace newCoeffs k (value */ v) in
  Hashtbl.iter f eq.coeffs;
  {coeffs    = newCoeffs;
   sup       = value */ eq.sup;
   isStrict  = eq.isStrict;
   nVar      = (if value =/ (num_of_int 0) then 0 else eq.nVar);
   previous  = eq.previous}

(* adds two equations *)
let add eq1 eq2 =
  let coeffs = Hashtbl.copy eq1.coeffs in
  let f k v =
    try
      let temp = Hashtbl.find coeffs k in
      Hashtbl.replace coeffs k (v +/ temp)
    with Not_found -> ()
  in
  let count _ v acc =
    if v <>/ Num.num_of_int 0 then acc+1 else acc
  in
  (* loop through the coeffs of the first equation*)
  Hashtbl.iter f eq2.coeffs;
  {coeffs = coeffs;
   sup = eq1.sup +/ eq2.sup;
   isStrict = eq1.isStrict && eq2.isStrict;
   nVar = Hashtbl.fold count coeffs 0; (* Count the variables. Not a problem since don't affect the complexity *)
   previous = eq1.previous @ eq2.previous
   (* TODO : can the previous overlapse ? It seems that yes in dejean's algo *)
  }

(* return the linear combination c1*eq1+c2*eq2 *)
let combine c1 eq1 c2 eq2 =
  add (multiply eq1 c1) (multiply eq2 c2)
