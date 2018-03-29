open General
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
        Print.print ["ITE",2] (fun p -> p "ITE: starting get loop");
        match K.what_now state with
        | Some(K.Sat msg), state ->   Msg msg, machine state
        | Some(K.Propa msg), state -> Msg msg, machine state
        | None, state -> Silence, machine state

      and machine state =
        Print.print ["ITE",2] (fun p -> p "ITE: starting machine loop");
        
        let add = function
          | None -> get state
          | Some sassign -> get(K.add sassign state)
        in

        let share tset = get(K.share tset state) in
        
        let clone () =
          Print.print ["ITE",2] (fun p -> p "ITE: cloning");
          machine state
        in

        let suicide _ = () in
        let propose ?term _ =
          let tset = K.wondering state in
          let term = TSet.choose tset in
          [boolassign term,1.0]
        in
        SlotMachine {add; share; clone; suicide; propose }

      in
      { PluginTh.init = machine K.init;
        PluginTh.clear = fun ()->() }

end
