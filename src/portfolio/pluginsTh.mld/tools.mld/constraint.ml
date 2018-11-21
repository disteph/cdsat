open General

open Kernel
open Top.Terms

open Theories.Theory
open Theories.Eq
open MyTheory

module type WatchStruct = sig
  include Top.Basic.PH
  val name : string
end

module K = Keys.Make()

module Record = Hashtbl_hetero.MakeS(K)
    (struct type ('a,_) t = (module Top.Basic.PH with type t = 'a) end)

let record : unit Record.t = Record.create 17

module Key = struct
  include K
  let make (type a) (module V: WatchStruct with type t = a) =
    let key = K.make (module V) in
    Record.add record key (module V);
    key
end

let next_id = ref 0

type t = WatchStruct : { id      : int;
                         key     : 'a Key.t;
                         watchstruct : 'a;
                         vkey    : 'b Top.Values.Key.t;
                         howmany : int;
                         among   : TSet.t;
                         watched : Term.t list;
                         valuation : sign Valuation.signed list } -> t


let pp_a (type a) (key : a Key.t) = let (module V) = Record.find record key in V.pp

let pp fmt (WatchStruct{ key; watchstruct; howmany; among; valuation }) =
  Format.fprintf fmt "Watching %i terms among %a in %a for Key_%a with valuation %a"
    howmany
    TSet.pp among
    (pp_a key) watchstruct
    Key.pp key
    (List.pp Valuation.pp) (List.map Valuation.reveal valuation)

let show = Format.stringOf pp

let id        (WatchStruct{ id }) = id
let among     (WatchStruct{ among }) = among
let howmany   (WatchStruct{ howmany }) = howmany
let watched   (WatchStruct{ watched }) = watched
let valuation (WatchStruct{ valuation }) =
  List.fold Valuation.sunion valuation Valuation.sempty
let watchstruct (type a) (k : a Key.t) (WatchStruct{ key; watchstruct }) : a =
  let Poly.Eq = Key.eq k key in watchstruct

let simplify (WatchStruct ws) (kstate,quid) =
  let { fixed; unknown; watchable }, kstate =
    kstate.watchfind ws.vkey ~howmany:ws.howmany ws.among in
  WatchStruct { ws with among     = unknown;
                        watched   = watchable;
                        valuation = fixed::ws.valuation },
  (kstate,quid)

let make key watchstruct vkey ~howmany ~among =
  let id = !next_id in
  incr next_id;
  WatchStruct
    { id ; key; watchstruct; vkey; howmany; among; watched = []; valuation = [] }
