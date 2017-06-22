(**********)
(* Values *)
(**********)

type 'v t =
  | NonBoolean of 'v
  | Boolean    of bool
                    [@@deriving eq, ord, show, hash]

let boolassign (t,b) = (t, Boolean b)
let bassign ?(b=true) t = boolassign(t,b)
  
