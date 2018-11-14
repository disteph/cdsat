open General
open Monads
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
       
type kstate = sign self

(* Configuration for the 2-watched literals module *)
module Config = struct
  module M = StateMonad(struct type t = kstate * Term.t list end)
  module Constraint = Constraint
  module Var = Term
  let simplify = Constraint.simplify

  let subscribe b term state = 
    let _,_,_,state = state.ask ~subscribe:b (Case1 term) in
    state
  
  let pick_another c i oldwatch (state,quid) =
    let newwatch = Constraint.watched c in
    let state = List.fold (subscribe false) oldwatch state in
    let state = List.fold (subscribe true) newwatch state in
    let quid  = List.rev_append newwatch quid in
    newwatch, (state, quid)
end

(* 2-watched literals module *)
module WL = TwoWatchedLits.Make(Config)

type state =
  | Unknown of {
      kstate : kstate;      (* Kernel state *)
      quid   : Term.t list; (* The newly watched terms we need to ask about *)
      wstate : WL.t;        (* The state of our watched literals *)
      sat    : (sign, sat) message option
    }
  | UNSAT of {
      propas : (sign,straight) imessage list; (* The propa messages we need to send*)
      unsat  : (sign,unsat) imessage;         (* Unsat message to send *)
    }
  | Done

let wrap f = function
  | UNSAT _ | Done as state -> state
  | Unknown{ kstate; wstate; quid } ->
    match f kstate with
    | MyTheory.UNSAT(propas,unsat)  ->
      let propas = List.rev propas in
      UNSAT{ propas; unsat }
    | MyTheory.SAT(sat,terms,kstate) ->
      let wstate = List.fold WL.fix terms wstate in
      Unknown{ sat = Some sat; kstate; wstate; quid}

let add sassign ~level = wrap (fun kstate -> kstate.add sassign ~level)

let share tset = wrap (fun kstate -> kstate.share tset)

let ask ?subscribe tv = function
  | UNSAT _ | Done as state -> None, state
  | Unknown{ kstate; wstate; quid; sat } ->
    let nf,cv,diff,kstate = kstate.ask ?subscribe tv in
    Some(nf,cv,diff),
    Unknown{ kstate; wstate; quid; sat }

let watchthis c = function
  | UNSAT _ | Done as state -> state
  | Unknown{ kstate; wstate; quid; sat } ->
    let randomterm = c |> Constraint.among |> TSet.choose in
    let rec randomwatch n = if n = 0 then [] else randomterm::(randomwatch (n-1)) in
    let ifpossible = c |> Constraint.howmany |> randomwatch in
    let wstate = WL.addconstraintNflag c ~ifpossible wstate in
    Unknown{ kstate; wstate; quid; sat }

type speak =
  | Nothing
  | Quid    : Term.t -> speak
  | IMsg    : (sign,_) imessage -> speak
  | Detect  : Constraint.t -> speak
    
let speak = function
  | Done -> Nothing, Done
  | UNSAT{ propas=[]; unsat }          -> IMsg unsat, Done
  | UNSAT{ propas=msg::propas; unsat } -> IMsg msg, UNSAT{ propas; unsat }
  | Unknown{ kstate; wstate; quid; sat } ->
    let (res,wstate), (kstate,quid) = WL.next wstate (kstate,quid) in
    match res with
    | Case2(c,_) -> Detect c, Unknown{ kstate; wstate; quid; sat }
    | Case1 _    ->
      match quid with
      | []      -> Nothing, Unknown{ kstate; wstate; quid; sat }
      | t::quid -> Quid t,  Unknown{ kstate; wstate; quid; sat }
  
module Make(Kern: API with type sign := MyTheory.sign) = struct
  
  let init = Unknown{ kstate = Kern.init; wstate = WL.init; quid = []; sat = None }
      
end
