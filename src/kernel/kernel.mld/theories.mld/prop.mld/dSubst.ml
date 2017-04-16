(*************************)
(* Delayed Substitutions *)
(*************************)

open Format
open General

open Top
open Variables

exception DSubst of string

type 'a aux = EmptySubst | ConsSubst of FreeVar.t*World.t*'a [@@deriving eq, hash]

module M = struct
  type 'a t = 'a aux [@@deriving eq, hash]
  let hash t = Hash.wrap1 hash_fold_t t
end

module H = HCons.Make(M)

include H
include Init(HCons.NoBackIndex)
            
let print_in_fmt fmt t =
  match reveal t with
  | EmptySubst -> ()
  | ConsSubst(fv,ar,s') ->
    let rec aux fmt t = match reveal t with
      | EmptySubst -> failwith "Should not happen"
      | ConsSubst(fv,ar,s') ->
        begin match reveal s' with
        | EmptySubst -> fprintf fmt "%a" FreeVar.print_in_fmt fv
        | _ -> fprintf fmt "%a;%a" FreeVar.print_in_fmt fv aux s'
        end
    in fprintf fmt "[%a]" aux t

let binit() = build EmptySubst

let init = binit()
let bind2FV (fv,ar) l = build (ConsSubst(fv,ar,l))

let get_arity d = match reveal d with
  | EmptySubst        -> World.init
  | ConsSubst(_,ar,_) -> ar

let rec get j d = match reveal d with
  | EmptySubst -> raise (DSubst 
                           (Dump.toString
                              (fun f -> f "Attempting to access bound variable %i in esubstitution %a" j print_in_fmt d)))
  | ConsSubst(fv,ar,d')
    -> if j=0 then (fv,ar) else get (j-1) d'

