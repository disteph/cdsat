open Kernel
open Export
open Top
open Specs
open Sassigns
open Theories.IfThenElse

type sign = MyTheory.sign

module Make(DS: GlobalImplem) = struct
  open DS

  let make (k: (Term.datatype,Value.t,Assign.t,TSet.t) MyTheory.api)
    = let (module K) = k in
      let rec get state =
        match K.what_now state with
        | Some(K.Sat msg), state ->   Msg msg, machine state
        | Some(K.Propa msg), state -> Msg msg, machine state
        | None, state ->
           let tset = K.wondering state in
           let term = TSet.choose tset in
           Try(boolassign term), machine state

      and machine state =
        
        let add = function
          | None -> get state
          | Some sassign -> get(K.add sassign state)
        in

        let share tset = Silence, machine(K.share tset state) in
        
        let clone () = machine state in

        let suicide _ = () in

        SlotMachine {add; share; clone; suicide }

      in
      { PluginTh.init = machine K.init;
        PluginTh.clear = fun ()->() }

end
