(**********************)
(* Single Assignments *)
(**********************)
open General
open Terms

type ('t,'v) sassign = SAssign : ('t * ('v,_) Values.t) -> ('t,'v) sassign
[@@unboxed] (* The constructor takes 1 argument (as a pair), not two *)

let is_Boolean (SAssign(_,v)) =
  match v with
  | Values.Boolean _ -> true
  | Values.NonBoolean _ -> false

(* Generic functions (will be used as such for Boolean assignments) *)

let equal_bassign equal_t equal_v (t1,v1) (t2,v2) =
  equal_t t1 t2 && Values.equal equal_v v1 v2

let compare_bassign compare_t compare_v (t1,v1) (t2,v2) =
  let res = compare_t t1 t2 in
  if res = 0 then Values.(compare compare_v v1 v2)
  else res

let hash_fold_bassign hash_fold_t hash_fold_v a =
  Hash.pair hash_fold_t (Values.hash_fold_t hash_fold_v) a

let pp_bassign (type t v a) pp_t pp_v fmt ((t,v) : t * (v,a) Values.t) =
  match v with
  | Values.Boolean b    ->
    if b then Format.fprintf fmt "%a" pp_t t
    else Format.fprintf fmt "~(%a)" pp_t t
  | Values.NonBoolean v -> Format.fprintf fmt "(%a↦ %a)" pp_t t pp_v v

let show_bassign pp_t pp_v = Format.stringOf (pp_bassign pp_t pp_v)
  
(* Functions for sassigns *)

let equal_sassign equal_t equal_v (SAssign sassign1) (SAssign sassign2) =
  equal_bassign equal_t equal_v sassign1 sassign2

let compare_sassign compare_t compare_v (SAssign sassign1) (SAssign sassign2) =
  compare_bassign compare_t compare_v sassign1 sassign2
                  
let hash_fold_sassign hash_fold_t hash_fold_v state (SAssign sassign) =
  hash_fold_bassign hash_fold_t hash_fold_v state sassign

let pp_sassign pp_t pp_v fmt (SAssign sassign) = pp_bassign pp_t pp_v fmt sassign

let show_sassign pp_t pp_v (SAssign sassign) = show_bassign pp_t pp_v sassign

(* let hash_sassign = Hash.fold2hash hash_fold_sassign *)

(* Boolean assignments *)
    
type ('t,'v) bassign = 't * ('v,bool) Values.t

let negation (t,Values.Boolean b) = (t,Values.Boolean(not b))

module BAssign = struct
  type t = (Term.t,Values.Value.t) bassign [@@deriving eq,ord,hash,show]
  let id (t,Values.Boolean b) = ((Term.id t)*2)+(if b then 0 else 1)
end

(* HConsed version of single assignments *)

module SAssign = struct

  include HCons.Make(struct type _ t = (Term.t,Values.Value.t) sassign end)
  module F = struct
    let equal _eqRec = equal_sassign Term.equal Values.Value.equal
    let hash_fold_t _hashRec = hash_fold_sassign Term.hash_fold_t Values.Value.hash_fold_t
    let name = "SAssign"
  end

  include Init(F)
  let pp fmt t = pp_sassign Term.pp Values.Value.pp fmt (reveal t)
  let show  = Print.stringOf pp

  let build s = build(SAssign s)
  let boolassign ?(b=true) t = build(t, Values.Boolean b)
  let is_Boolean = reveal >> is_Boolean
end

module Assign = Basic.MakePATCollection(SAssign)
