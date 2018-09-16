open General
open Kernel
open Top
open Terms
open Sassigns
open Theories.Theory
open Theories.IfThenElse

type sign = MyTheory.sign
type api  = (module MyTheory.API)

let hdl = MyTheory.hdl

module Make(W: Writable) = struct

  let make (module K: MyTheory.API) =
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
        if TSet.is_empty tset
        then []
        else
          let term = TSet.choose tset in
          [SAssign.boolassign term,1.0]
      in
      SlotMachine {add; share; clone; suicide; propose }

    in
    { PluginTh.init = machine K.init;
      PluginTh.clear = fun ()->() }

end
