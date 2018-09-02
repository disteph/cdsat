(*************************)
(* Delayed Substitutions *)
(*************************)
open General
open Variables

exception DSubst of string

type 'l t = ('l*World.t) list [@@deriving eq, hash, show]

let get_arity = function
  | [] -> World.init
  | (_,ar)::_ -> ar

let rec get ppl j d = match d with
  | [] -> raise (DSubst 
                   (Format.toString
                      (fun f -> f "Attempting to access bound variable %i in esubstitution %a" j (pp ppl) d)))
  | a::d'
    -> if j=0 then a else get ppl (j-1) d'

