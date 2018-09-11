open General

open Top
open Terms
open Messages
open Sassigns

open Theory

type sign = unit

module type API = sig
  val init : sign slot_machine
end

module T = struct
  let dskey = Termstructures.VarSet.Arrays.key
  let ds  = [DSK dskey]
  type nonrec sign = sign
  type api = (module API)
  let name = "Arrays"

  module Make(W : Writable) : API = struct

    let add_myvars = Terms.proj dskey >> TSet.fold TSet.add

    type state = { assign : Assign.t;
                   sharing: TSet.t;
                   myvars : TSet.t Lazy.t }

    let rec machine state =
      let add = function
        | None ->
          Print.print ["kernel.arrays",2] (fun p ->
              p "kernel.arrays receiving None");
          Silence, machine state
        | Some sassign ->
          Print.print ["kernel.arrays",2] (fun p ->
              p "kernel.arrays receiving Some(%a)" SAssign.pp sassign);
          let assign = Assign.add sassign state.assign in
          let SAssign(term,_) = SAssign.reveal sassign in
          let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
          let state = { state with assign; myvars } in
          Msg(sat () state.assign ~sharing:state.sharing ~myvars ),
          machine state
      in
      let share tset = 
        Print.print ["kernel.arrays",2] (fun p ->
            p "kernel.arrays notified than %a are shared" TSet.pp tset);
        let sharing = TSet.union tset state.sharing in
        let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
        let state = { state with sharing; myvars } in
        Msg(sat () state.assign ~sharing ~myvars),
        machine state
      in
      let clone () = machine state in
      let suicide _ = () in
      let propose ?term _ = [] in
      Theory.SlotMachine { add; share; clone; suicide; propose }

    let init = machine { assign=Assign.empty; sharing=TSet.empty; myvars=lazy TSet.empty }

  end
  
  let make (module W : Writable) : api = (module Make(W))

end

let hdl = register(module T)
