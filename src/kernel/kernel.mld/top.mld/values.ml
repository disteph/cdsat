(**********)
(* Values *)
(**********)

open General
       
type nbool = private NBool
       
type (_,_) t =
  | NonBoolean : 'v -> ('v,nbool) t
  | Boolean    : bool -> ('v,bool) t

type 'v isomorph = 'v option option [@@deriving eq,ord,hash]

let isomorph (type a) : ('v,a) t -> 'v isomorph = function
  | NonBoolean v -> Some(Some v)
  | Boolean true -> None
  | Boolean false -> Some None

let equal equal_v v v' = equal_isomorph equal_v (isomorph v) (isomorph v')
let compare compare_v v v' = compare_isomorph compare_v (isomorph v) (isomorph v')
                                            
let pp pp_v fmt (type a) : ('v,a) t -> unit =
  function
  | NonBoolean v -> pp_v fmt v
  | Boolean b -> Format.fprintf fmt "%b" b

let show pp_v = Print.stringOf (pp pp_v)

let hash_fold_t (type v) hash_fold_v state v =
  [% hash_fold: v isomorph] state (isomorph v)

let hash_fold (type t) hash_fold_t (type v) hash_fold_v state (t,v) =
  [% hash_fold: t*(v isomorph)] state (t,isomorph v)
