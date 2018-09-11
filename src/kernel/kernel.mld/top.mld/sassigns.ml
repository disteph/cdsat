(**********************)
(* Single Assignments *)
(**********************)
open General
open Terms
       
type sassign = SAssign : (Term.t * (Values.Value.t,_) Values.t) -> sassign
[@@unboxed] (* The constructor takes 1 argument (as a pair), not two *)

let is_Boolean (SAssign(_,v)) =
  match v with
  | Values.Boolean _ -> true
  | Values.NonBoolean _ -> false

(* Generic functions (will be used as such for Boolean assignments) *)

let equal_bassign (t1,v1) (t2,v2) =
  Term.equal t1 t2 && Values.(equal Value.equal v1 v2)

let compare_bassign (t1,v1) (t2,v2) =
  let res = Term.compare t1 t2 in
  if res = 0 then Values.(compare Value.compare v1 v2)
  else res

let hash_fold_bassign a =
  Hash.pair Term.hash_fold_t Values.(hash_fold_t Value.hash_fold_t) a

let hash_bassign = Hash.fold2hash hash_fold_bassign

(* Functions for sassigns *)

let equal_sassign (SAssign sassign1) (SAssign sassign2) =
  equal_bassign sassign1 sassign2

let compare_sassign (SAssign sassign1) (SAssign sassign2) =
  compare_bassign sassign1 sassign2
                  
let hash_fold_sassign state (SAssign sassign) =
  hash_fold_bassign state sassign

let hash_sassign = Hash.fold2hash hash_fold_sassign

(* Boolean assignments *)
    
type bassign = Term.t * (Values.Value.t,bool) Values.t

let pp_bassign fmt (t,Values.Boolean b) =
  if b then Format.fprintf fmt "%a" Term.pp t
  else Format.fprintf fmt "~(%a)" Term.pp t
let show_bassign = Print.stringOf pp_bassign
let negation (t,Values.Boolean b) = (t,Values.Boolean(not b))

(* Printing functions for sassigns *)

let pp_sassign fmt (SAssign((t,v) as poly)) =
  match v with
  | Values.Boolean _    -> pp_bassign fmt poly
  | Values.NonBoolean v -> Format.fprintf fmt "(%a↦ %a)" Term.pp t Values.Value.pp v

let show_sassign = Print.stringOf pp_sassign

(* HConsed version of single assignments *)

module SAssign = struct

  include HCons.Make(struct type _ t = sassign end)
  module F = struct
    let equal _eqRec = equal_sassign
    let hash_fold_t _hashRec = hash_fold_sassign
    let name = "SAssign"
  end

  include Init(F)
  let pp fmt t = pp_sassign fmt (reveal t)
  let show  = Print.stringOf pp

  let build s = build(SAssign s)
  let boolassign ?(b=true) t = build(t, Values.Boolean b)

end

module Assign = Basic.MakePATCollection(SAssign)
