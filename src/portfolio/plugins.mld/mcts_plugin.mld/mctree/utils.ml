let ( %> ) f g x = g (f x)

let ( % ) f g x = f (g x)

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let const x _ = x

external identity : 'a -> 'a = "%identity"

let tap f x = f x; x


let ( >>= ) x f = match x with None -> None | Some y -> f y


module Pair =
struct

let mapn f (x, y) = (f x, f y)

end

module Map =
struct

include (Map : module type of Map with module Make := Map.Make)

module Make(Ord : Map.OrderedType) =
struct
  include Map.Make(Ord)

let modify_def v0 k f m =
  try let y = find k m |> f in add k y m with Not_found -> add k (f v0) m


end

end


module Hashtbl =
struct

include Hashtbl

let modify_def def k f tbl =
  try Hashtbl.find tbl k |> f |> Hashtbl.replace tbl k
  with Not_found -> Hashtbl.add tbl k (f def)

end


module List =
struct (*$< List *)

include List

let cons x l = x :: l

let list_rest l =
  let rec go acc = function
    x :: xs -> (x, (acc, xs)) :: go (x :: acc) xs
  | [] -> [] in
  go [] l

(*$=
  (list_rest [1;2;3]) ([(1, ([], [2; 3])); (2, ([1], [3])); (3, ([2; 1], []))])
*)

(* `nth_rest i l` returns the i-th element of l as well as
   the elements before and after (in order)
*)
let nth_rest n =
  let rec go acc i = function
    x :: xs when i > 0 -> go (x :: acc) (i-1) xs
  | x :: xs when i = 0 -> x, (List.rev acc, xs)
  | _ -> failwith "nth_rest"
  in go [] n

(*$=
  (nth_rest 2 [3;4;5;6]) (5, ([3;4], [6]))
*)

let insert_at i elem l =
  let rec go acc = function
    (0, xs) -> List.rev_append acc (elem::xs)
  | (n, []) -> failwith "insert_at"
  | (n, x :: xs) -> go (x::acc) (n-1, xs)
  in go [] (i, l)

(*$=
  (insert_at 1 "foo" ["bar"; "baz"]) ["bar"; "foo"; "baz"]
*)

let take_drop n =
  let rec go acc i = function
    []      when i > 0 -> failwith "take_drop"
  | x :: xs when i > 0 -> go (x::acc) (i-1) xs
  | xs -> List.rev acc, xs
  in go [] n

let rec findi p l =
  let rec loop n = function
    | [] -> raise Not_found
    | h :: t ->
      if p n h then (n,h) else loop (n+1) t
  in loop 0 l

let concat_map f l = List.concat (List.map f l)

let fsum = List.fold_left (+.) 0.

let rec last = function
    [] -> failwith "last"
  | [h] -> h
  | h :: t -> last t

(* let rec union1 x y = match x with
 *     [] -> y
 *   | h :: t -> if List.mem h y then union1 t y else h :: union1 t y *)

(* (\* tail-recursive version -- reverses first argument *\)
 * let rec union2 x y = match x with
 *     [] -> y
 *   | h :: t -> if List.mem h y then union1 t y else union2 t (h :: y) *)

let rec fold_right1 f = function
    x :: [] -> x
  | x :: xs -> f x (fold_right1 f xs)
  | [] -> failwith "fold_right1"

let fold_left_map f acc = List.fold_left
  (fun (acc, ys) x -> let (acc', y) = f acc x in (acc', y :: ys)) (acc, [])

let fold_right_map f acc xs = List.fold_right
  (fun x (acc, ys) -> let (acc', y) = f acc x in (acc', y :: ys)) xs (acc, [])

let fold_map f sf l = let (sf, rev) = fold_left_map f sf l in sf, List.rev rev

let rec filter_map f = function
    [] -> []
  | x :: xs -> (match f x with None -> filter_map f xs | Some y -> y :: filter_map f xs)

let init stop f =
  let rec aux i = if i < stop then f i :: aux (i + 1) else []
  in aux 0

let rec interleave sep = function
    [] -> []
  | [x] -> [x]
  | x :: (_ :: _ as xs) -> x :: sep :: interleave sep xs

let min_max ?cmp:(cmp = Pervasives.compare) = function
  | [] -> invalid_arg "min_max"
  | x :: xs ->
      fold_left (fun (curr_min, curr_max) y ->
        let new_min = if cmp curr_min y =  1 then y else curr_min in
        let new_max = if cmp curr_max y = -1 then y else curr_max in
        (new_min, new_max))
      (x, x) xs

let reduce f = function
    [] -> invalid_arg "reduce"
  | h::t -> fold_left f h t

(* let min l = reduce Pervasives.min l
 * let max l = reduce Pervasives.max l *)

(* let rec unique = function
 *     [] -> []
 *   | x :: xs -> if List.mem x xs then xs else x :: unique xs *)

let rec modify_def v0 k f = function
    [] -> [k, f v0]
  | (k', v') as x :: xs ->
      if k = k' then (k, f v') :: xs else x :: modify_def v0 k f xs

end (*$>*)


module IO =
struct

let rec lines_of c = try input_line c :: lines_of c with End_of_file -> []

end


module File =
struct

let with_file_in path f =
  let c = open_in path in
  let y = f c in
  close_in c; y

let with_file_out path f =
  let c = open_out path in
  f c; close_out c

let lines_of path = with_file_in path IO.lines_of

end


module Float =
struct

let add x y = x +. y
let mul x y = x *. y

let neg x = -. x

end


module String =
struct

include String

let lchop n s = String.sub s n (String.length s - n)

let filter f s =
  let len = length s in
  let sc = Buffer.create len in
  for i = 0 to len - 1 do
    let c = unsafe_get s i in
    if f c then Buffer.add_char sc c
  done;
  Buffer.contents sc

end


module Option =
struct

let map f = function None -> None | Some x -> Some (f x)

let is_none = function None -> true | Some _ -> false
let is_some = function None -> false | Some _ -> true

let get = function None -> failwith "get" | Some x -> x

let default v0 = function None -> v0 | Some x -> x

end
