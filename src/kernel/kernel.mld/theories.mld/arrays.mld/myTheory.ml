open General

open Top
open Terms
open Messages
open Sassigns

open Theory

type sign = unit

type state = { assign : Assign.t;
               sharing: TSet.t;
               myvars : TSet.t Lazy.t }

module Monad = Monads.StateMonad(struct type t = state end)

module type API = sig
  val add   : SAssign.t option -> (sign,sat) message option Monad.t
  val share : TSet.t -> (sign,sat) message option Monad.t
  val init  : state
end

module T = struct
  let dskey = Termstructures.VarSet.Arrays.key
  let ds  = [DSK dskey]
  type nonrec sign = sign
  type api = (module API)
  let name = "Arrays"

  module Make(W : Writable) : API = struct

    let add_myvars = Terms.proj dskey >> TSet.fold TSet.add

    let add sassign state = match sassign with
      | None ->
        Print.print ["kernel.arrays",2] (fun p ->
            p "kernel.arrays receiving None");
        None, state
      | Some sassign ->
        Print.print ["kernel.arrays",2] (fun p ->
            p "kernel.arrays receiving Some(%a)" SAssign.pp sassign);
        let assign = Assign.add sassign state.assign in
        let SAssign(term,_) = SAssign.reveal sassign in
        let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
        let state = { state with assign; myvars } in
        Some(sat () state.assign ~sharing:state.sharing ~myvars ),
        state

    let share tset state = 
      Print.print ["kernel.arrays",2] (fun p ->
          p "kernel.arrays notified than %a are shared" TSet.pp tset);
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      let state = { state with sharing; myvars } in
      Some(sat () state.assign ~sharing ~myvars),
      state

    let init = { assign=Assign.empty; sharing=TSet.empty; myvars=lazy TSet.empty }

  end
  
  let make (module W : Writable) : api = (module Make(W))

end

let hdl = register(module T)
