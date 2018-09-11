open General
open Top.Terms
include Theory_sig

module K = Keys.Make()

module Tags = struct
  type 'a t = {
    key    : 'a K.t;
    dsKeys : dsKey list;
    make   : (module Writable) -> 'api
  } constraint 'a = _*'api
    [@@deriving fields]
  
  let id k = k |> key |> K.id
  let hash = id
  let compare k1 k2 = K.compare (key k1) (key k2)
  let equal k1 k2   = K.equal (key k1) (key k2)
  let eq k1 k2      = K.eq (key k1) (key k2)
  let pp fmt = key >> K.pp fmt
end

module Handlers = struct

  type t =
    | Handler : _ Tags.t -> t
    | Eq

  let id = function
    | Handler hdl -> Tags.id hdl +1
    | Eq -> 0

  let compare = Compare.id2compare id
  let pp fmt = function
    | Handler hdl -> Tags.pp fmt hdl
    | Eq -> Format.fprintf fmt "Eq"
  let show = Print.stringOf pp

end

let all_theories_list = ref [Handlers.Eq]

let register (type sign) (type api)
    (module T: Type with type sign = sign and type api = api)
  = let key = K.make (module struct include T type t = sign*api end) in
  let tag = {Tags.key; Tags.dsKeys = T.ds ; Tags.make = T.make} in
  all_theories_list := (Handlers.Handler tag)::!all_theories_list;
  tag

let fail_state =
  let add _ = failwith "Are you dumb? I already told you it was provable" in
  let share, clone, suicide = add, add, add in
  let propose ?term i = add i in
  SlotMachine { add; propose; share; clone; suicide }
