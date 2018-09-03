(**********)
(* Values *)
(**********)

open General
       
type nbool = private NBool
       
type (_,_) t =
  | NonBoolean : 'v -> ('v,nbool) t
  | Boolean    : bool -> ('v,bool) t

let pp pp_v fmt (type a) : ('v,a) t -> unit =
  function
  | NonBoolean v -> pp_v fmt v
  | Boolean b -> Format.fprintf fmt "%b" b

let show pp_v = Print.stringOf (pp pp_v)


type 'v isomorph = 'v option option [@@deriving eq,ord,hash]

let isomorph (type a) : ('v,a) t -> 'v isomorph = function
  | NonBoolean v -> Some(Some v)
  | Boolean true -> None
  | Boolean false -> Some None

let equal equal_v v v' = equal_isomorph equal_v (isomorph v) (isomorph v')
let compare compare_v v v' = compare_isomorph compare_v (isomorph v) (isomorph v')

let hash_fold_t (type v) hash_fold_v state v =
  [%hash_fold: v isomorph] state (isomorph v)

(* Values (existentially quantified on their Booleanness) *)

type 'v values = Values : ('v,_) t -> 'v values [@@unboxed]
let equal_values equal_v (Values v1) (Values v2) = equal equal_v v1 v2
let compare_values compare_v (Values v1) (Values v2) = compare compare_v v1 v2                                                               
let hash_fold_values hash_fold_v state (Values v) =
  hash_fold_t hash_fold_v state v
let pp_values pp_v fmt (Values v) = pp pp_v fmt v
let show_values pp_v = Print.stringOf (pp_values pp_v)

(************************)
(* Values that are used *)
(************************)

module type Value = sig
  include Basic.PH
  val name : string
end

module K = Keys.Make()

module Record = Hashtbl_hetero.MakeS(K)
    (struct type ('a,_) t = (module Basic.PH with type t = 'a) end)

let record = Record.create 17

module Value = struct
      
  type t = Pair : 'a K.t * 'a -> t

  let equal (Pair(k1,v1)) (Pair(k2,v2)) =
    match K.equal k1 k2 with
    | Poly.Eq -> let (module V) = Record.find record k1 in V.equal v1 v2
    | Poly.Neq-> false

  let compare (Pair(k1,v1)) (Pair(k2,v2)) =
    match K.equal k1 k2 with
    | Poly.Eq -> let (module V) = Record.find record k1 in V.compare v1 v2
    | Poly.Neq-> K.compare k1 k2

  let hash_fold_t state (Pair(k,v)) =
    let (module V) = Record.find record k in
    [%hash_fold:int*V.t] state (K.id k,v)

  let hash = Hash.fold2hash hash_fold_t
      
  let pp fmt (Pair(k,v)) =
    let (module V) = Record.find record k in
    V.pp fmt v

  let show = Print.stringOf pp

end

module Key = struct
  include K
  let make (type a) (module V: Value with type t = a) =
    let key = K.make (module V) in
    let inj value = Value.Pair(key,value) in
    let proj (Value.Pair(key',value)) =
      match K.equal key key' with
      | Poly.Eq -> Some (value:a)
      | Poly.Neq-> None
    in
    Record.add record key (module V);
    key, inj, proj
end

module CValue = struct
  include Hashtbl_hetero.MakeT(Key)
  let proj key cvalue = if mem cvalue key then Some(find cvalue key) else None
  let printk = {printk = Key.pp}
  let printd (type a) (key : a Key.t) = let (module V) = Record.find record key in V.pp
  let printd = {printd}
  let pp = pp ";" "," printk printd
  let show = Print.stringOf pp
end

