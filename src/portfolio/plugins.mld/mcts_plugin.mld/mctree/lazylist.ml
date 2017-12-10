(*
module Lazylist =
struct
*)

type 'x t = unit -> 'x node_t
and 'x node_t = Nil | Cons of ('x * 'x t)

let nil () = Nil
let cons x xs () = Cons (x, xs)

let next xs = xs ()

let rec of_list xs () = match xs with
    [] -> Nil
  | x :: xs -> Cons(x, of_list xs)

let rec to_list xs = match xs () with
    Nil -> []
  | Cons (x, xs) -> x :: to_list xs

let peek xs = match xs () with
    Nil -> None
  | Cons (x, _) -> Some x

let get xs = match xs () with
    Nil -> None
  | Cons (x, xs) -> Some (x, xs)

let rec map f xs () = match xs () with
    Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

let rec mapi f l =
  let rec aux i l () = match l () with
    Nil -> Nil
  | Cons (x, xs) -> Cons (f i x, aux (i + 1) xs)
  in aux 0 l

let append (l1 : 'a t) (l2 : 'a t) =
  let rec aux l () = match l () with
      Cons (x, (t : 'a t)) -> Cons (x, aux t)
    | _                    -> l2 ()
  in aux l1

let rec filter p xs () = match xs () with
    Nil -> Nil
  | Cons (x, xs) ->
      if p x then Cons (x, filter p xs)
      else filter p xs ()

let rec filter_map f xs () = match xs () with
    Nil -> Nil
  | Cons (x, xs) -> (match f x with
        None -> filter_map f xs ()
      | Some y -> Cons (y, filter_map f xs))

let rec exists p xs = match xs () with
    Nil -> false
  | Cons (x, xs) -> if p x then true else exists p xs

let lazy_fold_right f l init =
  let rec aux rest () =
    match rest () with
    | Cons (x, t) -> f x (aux t)
    | Nil -> init ()
  in aux l

let concat lol =
  lazy_fold_right (fun li rest -> append li rest ()) lol nil

let concat_map f l = concat (map f l)

(*
let concat lol =
  let rec aux rest () =
    match rest () with
    | Cons (x, t) -> append x (aux t) ()
    | Nil -> nil () in
  aux lol
*)

let rec length xs = match xs () with
    Nil -> 0
  | Cons (x, xs) -> 1 + length xs

let rec from_loop f acc () =
  match f acc with
    None -> Nil
  | Some (x, acc') -> Cons (x, from_loop f acc')

let last l =
  let rec aux acc l = match next l with
      Nil        -> acc
    | Cons(x, t) -> aux (Some x) t
  in aux None l

let rec take n li =
  if n = 0 then fun () -> Nil
  else fun () ->
    (match li () with
      Nil -> Nil
    | Cons (x, xs) -> Cons (x, take (n - 1) xs))

let rec range start stop =
  if start > stop then fun () -> Nil
  else fun () -> Cons (start, range (start + 1) stop)

(*
end
*)
