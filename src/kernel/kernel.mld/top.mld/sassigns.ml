(**********************)
(* Single Assignments *)
(**********************)
open General
       
type ('t,'v) sassign = SAssign : ('t * ('v,_) Values.t) -> ('t,'v) sassign [@@unboxed]

let is_Boolean (SAssign(_,v)) =
  match v with
  | Values.Boolean _ -> true
  | Values.NonBoolean _ -> false
      
let equal_bassign equal_t equal_v (t1,v1) (t2,v2) =
  equal_t t1 t2 && Values.equal equal_v v1 v2

let compare_bassign compare_t compare_v (t1,v1) (t2,v2) =
  let tmp = compare_t t1 t2 in
  if tmp=0 then Values.compare compare_v v1 v2
  else tmp

let equal_sassign equal_t equal_v (SAssign sassign1) (SAssign sassign2) =
  equal_bassign equal_t equal_v sassign1 sassign2

let compare_sassign compare_t compare_v (SAssign sassign1) (SAssign sassign2) =
  compare_bassign compare_t compare_v sassign1 sassign2
                  
let hash_fold_sassign hash_fold_t hash_fold_v state (SAssign(t,v)) =
  Values.hash_fold hash_fold_t hash_fold_v state (t,v)
    
type ('t,'v) bassign = 't * ('v,bool) Values.t
let hash_fold_bassign = Values.hash_fold             
let pp_bassign pp_t _ fmt (t,Values.Boolean b) =
  if b then Format.fprintf fmt "%a" pp_t t
  else Format.fprintf fmt "~(%a)" pp_t t
let show_bassign pp_t pp_v = Print.stringOf (pp_bassign pp_t pp_v)
let negation (t,Values.Boolean b) = (t,Values.Boolean(not b))
let boolassign ?(b=true) t = SAssign(t, Values.Boolean b)

let pp_sassign pp_t pp_v fmt (SAssign((t,v) as poly)) =
  match v with
  | Values.Boolean _    -> pp_bassign pp_t pp_v fmt poly
  | Values.NonBoolean v -> Format.fprintf fmt "(%a↦ %a)" pp_t t pp_v v

let show_sassign pp_t pp_v = Print.stringOf (pp_sassign pp_t pp_v)


type 'v values = Values : ('v,_) Values.t -> 'v values [@@unboxed]
let equal_values equal_v (Values v1) (Values v2) = Values.equal equal_v v1 v2
let compare_values compare_v (Values v1) (Values v2) = Values.compare compare_v v1 v2                                                               
let hash_fold_values hash_fold_v state (Values v) =
  Values.hash_fold_t hash_fold_v state v
let pp_values pp_v fmt (Values v) = Values.pp pp_v fmt v
let show_values pp_v = Print.stringOf (pp_values pp_v)
