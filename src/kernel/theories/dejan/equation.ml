include Num
include Hashtbl

open Num

type var = int
type value = num

(* Represents an inequation *)
type equation = {coeffs : (var, value) Hashtbl.t;  (* Coeffs *)
                 sup : value;                      (* Sup value *)
                 isStrict : bool;                  (* Is the inequality <= ? *)
                 guardians : var option * var option;
                 previous : equation list          (* equation whose linear combination allowed to find this one *)
                }


(* Pretty print an equation *)
let print eq =
  Hashtbl.iter (fun k v ->
    if (not(v =/ num_of_int 0)) then
      begin
      if (v =/ num_of_int 1) then
          Printf.printf "x%i + " k
        else begin
          if (v =/ num_of_int (-1)) then
            Printf.printf "-x%i + " k
          else
            Printf.printf "%s*x%i + " (string_of_num v) k;
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

exception Var_found of var

(* return an active variable, eg a variable of the equation whose coeff is non-nul*)
let getActiveVar eq = match eq.guardians with
  | None, None -> print_string "Error"; raise Not_found
  | Some(a), _ | _, Some(a) -> a

let getAnotherActiveVar eq v = match eq.guardians with
  | None, None -> raise Not_found
  | None, Some(a) | Some(a), None -> if a = v then raise Not_found else a
  | Some(a), Some(b) -> if a = v then b else a

let getActiveVars eq =
  let f k v accu =
    if v <>/ (Num.num_of_int 0) then k::accu else accu
  in
  Hashtbl.fold f eq.coeffs []

(* Look for an active variable through an equation, différent from the one given *)
let searchAnotherActiveVar coeff variable =
  let f k v =
    if k <> variable && v <>/ (num_of_int 0) && v =/ Hashtbl.find coeff k then raise (Var_found k)
  in try Hashtbl.iter f coeff; None with Var_found k -> Some(k)

let initGuardians coeffs =
  let g1 = ref None and g2 = ref None in
  let f k v =
    if v <>/ (Num.num_of_int 0) && v =/ Hashtbl.find coeffs k then match !g1 with
      | None -> g1 := Some(k)
      | _ -> g2 := Some(k); raise (Var_found k)
  in
  (try Hashtbl.iter f coeffs with Var_found k -> ());
    !g1, !g2

let updateGuardians coeffs guardians =
  match guardians with
  | None, None -> guardians
  | Some(a), None | None, Some(a) -> if ((Hashtbl.mem coeffs a) && Hashtbl.find coeffs a <>/ (num_of_int 0)) then guardians else None, None
  | Some(a), Some(b) ->
    let newa = if ((Hashtbl.mem coeffs a) && Hashtbl.find coeffs a <>/ (num_of_int 0)) then
        Some(a)
      else searchAnotherActiveVar coeffs b
    in
    let newb = if ((Hashtbl.mem coeffs b) && Hashtbl.find coeffs b <>/ (num_of_int 0)) then
        Some(b)
      else searchAnotherActiveVar coeffs a
    in newa, newb

(* Creates an equation from its subparts *)
let create coeffs sup isStrict previous =
  {coeffs    = coeffs;
   sup       = sup;
   isStrict  = isStrict;
   guardians = initGuardians coeffs;
   previous  = previous}

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

let toggleStrict eq =
   isStrict  = (not eq.isStrict);
   guardians = eq.guardians;
   previous  = eq.previous}

(* TODO should we directly implement this while constructing the equations ?*)
let getPrevious eq =
  match eq.previous with
  | [] -> [eq]
  | _ -> eq.previous

(* Add equations to previous equations *)
let addDependance eq ll =
  {coeffs    = eq.coeffs;
   sup       = eq.sup;
   isStrict  = eq.isStrict;
   guardians = eq.guardians;
   previous  = eq.previous@ll}

(* Change equations to previous equations *)
let setDependance eq ll =
  {coeffs    = eq.coeffs;
   sup       = eq.sup;
   isStrict  = eq.isStrict;
   guardians = eq.guardians;
   previous  = ll}

let uniq x =
   let rec uniqRec x accu =
     match x with
     | [] -> accu
     | t::q when List.mem t accu -> uniqRec q accu
     | t::q -> uniqRec q (t::accu)
   in
   uniqRec x []


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
       guardians = updateGuardians newCoeffs eq.guardians;
       previous = [eq]}
    ) with Not_found ->
      {coeffs   = eq.coeffs;
       sup      = eq.sup;
       isStrict = eq.isStrict;
       guardians = eq.guardians;
       previous = [eq]}


let rec affectVars eq l = match l with
  |  [] -> eq
  |  (vr,vl)::q -> let eq1 = affectVar eq vr vl in
                      affectVars eq1 q

(* Return true if and only if the eqation is an atomic constraint*)
let isAtomic eq = match eq.guardians with
  | Some(_), None | None, Some(_) -> true
  | _ -> false

(* Return true if and only if the eqation is a trivial inequation *)
let isTrivial eq = match eq.guardians with
  | None, None -> true
  | _ -> false
  (*eq.guardians == (None, None)*)


let isContradictory eq =
  isTrivial eq && ( (eq.isStrict && eq.sup <=/ Num.num_of_int 0 )
                    || (not(eq.isStrict) && eq.sup </ Num.num_of_int 0 ) )


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
   guardians = updateGuardians newCoeffs eq.guardians;
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
  (* loop through the coeffs of the first equation*)
  Hashtbl.iter f eq2.coeffs;
  {coeffs = coeffs;
   sup = eq1.sup +/ eq2.sup;
   isStrict = eq1.isStrict && eq2.isStrict;
   guardians = updateGuardians coeffs eq1.guardians;
   previous = []
   (* we do not need to update previous. (see the algorithm :
   fourierMotzkin resolution handle it itself)*)
  }

(* return the linear combination c1*eq1+c2*eq2 *)
let combine c1 eq1 c2 eq2 =
  add (multiply eq1 c1) (multiply eq2 c2)
