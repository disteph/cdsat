exception Abort

let abort = fun () ->
  raise Abort

let unopt = fun ?default x ->
  match x, default with
    | Some x, _        -> x
    | None  ,   Some x -> x
    | None  ,   None   -> failwith "[unopt]"

let fopt = fun f -> function
  | None   -> None
  | Some x -> Some (f x)

let fopt_p = fun f -> function
  | None   -> None
  | Some x -> f x

let fopt2_left = fun f x y ->
  match x with
  | None   -> y
  | Some x -> f x y

let fopt2 = fun f x y ->
  match x, y with
  | None  , None   -> None
  | Some x, None   -> Some x
  | None  , Some y -> Some y
  | Some x, Some y -> Some (f x y)

(* -------------------------------------------------------------------- *)
module StringComparable =
struct
  type t       = string
  let  compare = (Pervasives.compare : t -> t -> int)
end

module StringSet = Set.Make(StringComparable)

module ExtMapMake(X : Map.OrderedType) =
struct
  include Map.Make(X)

  module Set = Set.Make(X)

  let items = fun t ->
    List.rev (fold (fun k v items -> (k, v) :: items) t [])

  let keys = fun t ->
    fold (fun k v keys -> Set.add k keys) t Set.empty
end

module StringMap = ExtMapMake(StringComparable)

(* -------------------------------------------------------------------- *)
module Array =
struct
  include Array

  exception StopIteration

  let init_matrix = fun n m f ->
    init n (fun i -> init m (fun j -> f i j))

  let inmap = fun f array ->
    iteri
      (fun i x -> unsafe_set array i (f i x))
      array

  let foldi_left f init array =
    let result = ref init in
      iteri (fun i x -> result := f !result i x) array;
      !result

  let iteri_b = fun f array ->
    try  iteri f array
    with StopIteration -> ()

  let fall = fun test array ->
    let result = ref true in
    let test   = fun _ v ->
      if not (test v) then begin
        result := false; raise StopIteration
      end
    in
      iteri_b test array; !result

  let efind = fun test array ->
    let result = ref None in
    let test   = fun i v ->
      match test i v with
      | Some x ->
          result := Some (i, v, x); raise StopIteration
      | None   -> ()
    in
      iteri_b test array; !result

  let find = fun test array ->
    let result = ref None in
    let test   = fun i v ->
      if test i v then begin
        result := Some (i, v); raise StopIteration
      end
    in
      iteri_b test array; !result
end

(* -------------------------------------------------------------------- *)
module Big_int =
struct
  include Big_int

  let bint_0 = zero_big_int
  let bint_1 = unit_big_int

  let lcm = fun x y ->
    div_big_int (mult_big_int x y) (gcd_big_int x y)
end

(* -------------------------------------------------------------------- *)
module Num =
struct
  include Num

  let num_0     = (num_of_int   0)
  let num_1     = (num_of_int   1)
  let num_2     = (num_of_int   2)
  let num_neg_1 = (num_of_int (-1))

  let bigint_num = function
  | Int     n -> Big_int.big_int_of_int n
  | Big_int n -> n
  | Ratio   n -> Ratio.big_int_of_ratio n

  let decompose_num = fun x ->
    let x = Ratio.normalize_ratio (ratio_of_num x) in
      (Ratio.numerator_ratio x, Ratio.denominator_ratio x)
end
