open General
open Sums
open Patricia
open Patricia_tools
       
open Kernel
open Top.Terms
open Top.Sassigns
open Top.Messages

open Theories.Theory
open Theories.Eq
open MyTheory

open Tools
       
type sign = MyTheory.sign

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

type kstate = sign self
  
module Constraint : sig
  type t [@@deriving show]
  val id      : t -> int
  val watchstruct : 'a Key.t -> t -> 'a
  val among   : t -> TSet.t
  val watched   : t -> Term.t list
  val valuation : t -> sign Valuation.signed
  val make    : 'a Key.t -> 'a -> _ Top.Values.Key.t -> howmany:int -> among:TSet.t -> t
  val simplify: kstate -> t -> t
end = struct

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
  let watched   (WatchStruct{ watched }) = watched
  let valuation (WatchStruct{ valuation }) =
    List.fold Valuation.sunion valuation Valuation.sempty
  let watchstruct (type a) (k : a Key.t) (WatchStruct{ key; watchstruct }) : a =
    let Poly.Eq = Key.eq k key in watchstruct

  let simplify state (WatchStruct ws) =
    let { fixed; unknown; watchable }, state =
      state.watchfind ws.vkey ~howmany:ws.howmany ws.among in
    WatchStruct { ws with among     = unknown;
                          watched   = watchable;
                          valuation = fixed::ws.valuation } 

  let make key watchstruct vkey ~howmany ~among =
    let id = !next_id in
    incr next_id;
    WatchStruct
      { id ; key; watchstruct; vkey; howmany; among; watched = []; valuation = [] }
end

(* Configuration for the 2-watched literals module *)
module Config = struct
  module Constraint = Constraint
  module Var = Term
  type fixed = kstate
  let simplify = Constraint.simplify
  let pick_another _ c i _ = Constraint.watched c
end

(* 2-watched literals module *)
module WL = TwoWatchedLits.Make(Config)

type state =
  | Unknown of {
      kstate : kstate; (* Kernel state *)
      wstate : WL.t;  (* The state of our watched literals *)
      sat    : (sign, sat) message option
    }
  | UNSAT of {
      propas : (sign,straight) imessage list; (* The propa messages we need to send*)
      unsat  : (sign,unsat) imessage option;  (* Unsat message to send *)
    }
  | Done

let wrap f = function
  | UNSAT _ | Done as state -> state
  | Unknown{ kstate; wstate } ->
    match f kstate with
    | MyTheory.UNSAT(propas,unsat)  ->
      let propas = List.rev propas in
      UNSAT{ propas; unsat = Some unsat }
    | MyTheory.SAT(sat,terms,kstate) ->
      let wstate = List.fold WL.fix terms wstate in
      Unknown{ sat = Some sat; kstate; wstate}

let add sassign ~level = wrap (fun kstate -> kstate.add sassign ~level)

let share tset = wrap (fun kstate -> kstate.share tset)

let watchthis key watchstruct vkey ~howmany ~among state = function
  | UNSAT _ | Done as state -> state
  | Unknown{ kstate; wstate; sat } ->
    let c = Constraint.make key watchstruct vkey ~howmany ~among in
    let randomterm = TSet.choose among in
    let rec randomwatch n = if n = 0 then [] else randomterm::(randomwatch (n-1)) in
    let wstate = WL.addconstraintNflag c ~ifpossible:(randomwatch howmany) wstate in
    Unknown{ kstate; wstate; sat }
      
module Make(K: API with type sign = MyTheory.sign) = struct
  
  let init = Unknown{ kstate = K.init; wstate = WL.init; sat = None }
      
end
