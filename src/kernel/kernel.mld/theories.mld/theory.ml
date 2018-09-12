open General
open Top.Terms
include Theory_sig

module K = Keys.Make()

module Arg = struct
  type ('a,_) t = R :{
      dsKeys : dsKey list;
      make   : (module Writable) -> 'api
    } -> (_*'api,_) t
end

module Record = Hashtbl_hetero.MakeS(K)(Arg)

let record = Record.create 17

module Tags = struct
  include K
  let dsKeys key =
    let Arg.R{dsKeys} = Record.find record key in
    dsKeys
  let make key =
    let Arg.R{make} = Record.find record key in
    make
end

module Handlers = struct

  type t =
    | Handler : (_*_) Tags.t -> t
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
  Record.add record key (Arg.R{dsKeys = T.ds; make = T.make });
  all_theories_list := (Handlers.Handler key)::!all_theories_list;
  key

let fail_state =
  let add _ = failwith "Are you dumb? I already told you it was provable" in
  let share, clone, suicide = add, add, add in
  let propose ?term i = add i in
  SlotMachine { add; propose; share; clone; suicide }
