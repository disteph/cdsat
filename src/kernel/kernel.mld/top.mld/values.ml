(**********)
(* Values *)
(**********)

open General
       
type nbool = private NBool
       
type (_,_) t =
  | NonBoolean : 'v -> ('v,nbool) t
  | Boolean    : bool -> ('v,bool) t

let pp pp_v fmt (type a) : ('v,a) t -> unit = function
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

module Key = struct
  include K
  let make (type a) (module V: Value with type t = a) =
    let key = K.make (module V) in
    Record.add record key (module V);
    key
end

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

  let inj key value = Pair(key,value)

  let proj (type a) (key: a Key.t) (Pair(key',value)) =
    match K.equal key key' with
    | Poly.Eq -> Some (value:a)
    | Poly.Neq-> None

end

let inj key = function
  | Values(Boolean b) -> Values(Boolean b)
  | Values(NonBoolean value) -> Values(NonBoolean(Value.inj key value))

module CValue = struct
  module Arg = struct
    type 'a t = 'a Key.t
    module Int = struct
      type t = int
      let id i = i
    end
    include Patricia_tools.TypesFromHConsed(Int)
    let tag = Key.id
    let compare (type a1 a2) (k1 : a1 Key.t) (k2 : a2 Key.t) : (a1,a2)Poly.ord =
      match Key.equal k1 k2 with
      | Poly.Eq  -> Poly.Eq
      | Poly.Neq -> if Key.compare k1 k2 < 0 then Poly.Lt else Poly.Gt

    let hash_fold_t state k = Hash.hash2fold Key.hash state k
    
    type 'a values = 'a
    let equal_values (type a) (k: a t) v1 v2 =
      let (module V) = Record.find record k in V.equal v1 v2
    let hash_fold_values (type a) (k: a t) =
      let (module V) = Record.find record k in V.hash_fold_t
  end

  module CV = Map_hetero.MakeH(Arg)

  open Sums
      
  type t = (Boolhashed.t option,CV.t) sum  [@@deriving eq,ord,hash]

  let pp =
    let print_pair (type a) fmt ((k,v) : a Key.t * a) =
      let (module V) = Record.find record k in V.pp fmt v
    in
    fun fmt -> function
    | Case1(Some b) -> Format.fprintf fmt "%b" b
    | Case1 None    -> Format.fprintf fmt "%s" "NoBval"
    | Case2 cv -> CV.print_in_fmt {print_pair} fmt cv

  let show = Print.stringOf pp

  let none = function
    | Sorts.Prop -> Case1 None
    | _ -> Case2 CV.empty

  let inj (Values v) = match v with
    | Boolean b                   -> Case1(Some b)
    | NonBoolean(Value.Pair(k,v)) -> Case2(CV.singleton k v)
  
  let proj (type a) (key : a Key.t) = function
    | Case1(Some b) -> Some(Values(Boolean b))
    | Case1 None    -> None
    | Case2 cvalue  ->
      if CV.mem key cvalue then Some(Values(NonBoolean(CV.find key cvalue)))
      else None

  let fold_action =
    let sameleaf (type a) k v1 v2 () =
      if Arg.equal_values k v1 v2 then Case2(CV.singleton k v1)
      else Case1(Value.inj k v1, Value.inj k v2)
    in
    let emptyfull b () = Case2 b in
    let fullempty a () = Case2 a in
    let union = CV.{union = fun _ _ _ -> failwith "CValue union"} in
    let combine ~reccall cv1 cv2 = function
      | Case1 _ as ex -> ex
      | Case2 cv -> match reccall cv1 cv2 () with
        | Case1 _ as ex -> ex
        | Case2 cv' -> Case2(CV.union union cv cv')
    in
    let combine = CV.Fold2.make_combine combine in
    CV.Fold2.{sameleaf; emptyfull; fullempty; combine}
  
  let merge v1 v2 : (Value.t values*Value.t values,t) sum =
    match v1, v2 with
    | Case1 None, Case1 b
    | Case1 b, Case1 None
      -> Case2(Case1 b)
    | Case1(Some b1), Case1(Some b2) when not([%eq:bool] b1 b2)
      -> Case1(Values(Boolean b1), Values(Boolean b2))
    | Case1(Some _), Case1(Some _)
      -> Case2 v1
    | Case1 _, Case2 _
    | Case2 _, Case1 _
      -> failwith "Comparing Booleans with non-booleans"
    | Case2 v1, Case2 v2
      -> match CV.fold2 ~equal:(fun a () -> Case2 a) fold_action v1 v2 () with
      | Case1(v1,v2)
        -> Case1(Values(NonBoolean v1),Values(NonBoolean v2))
      | Case2 v -> Case2(Case2 v)
                     
end

