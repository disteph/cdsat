(**********)
(* Values *)
(**********)

type 'v t =
  | NonBoolean of 'v
  | Boolean    of bool
                    [@@deriving eq, show]
