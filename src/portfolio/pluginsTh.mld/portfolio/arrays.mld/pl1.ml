open Kernel
open Top.Terms
open Theories.Arrays

open PluginTh
    
type sign = MyTheory.sign
type api  = (module MyTheory.API)

let hdl = MyTheory.hdl
  
module Make(W: Writable) = struct

  let make (module K : MyTheory.API) =
    let rec machine state =
      let wrap = function
        | Some ans, state -> Msg ans, machine state
        | None, state -> Silence, machine state
      in
      let add sassign = K.add sassign state |> wrap in
      let share tset = K.share tset state |> wrap in
      let propose ?term int = [] in
      let clone () = machine state in
      let suicide _ = () in
      SlotMachine{ add; share; propose; clone; suicide }
    in
    {
      PluginTh.init  = machine K.init;
      PluginTh.clear = (fun () -> ())
    }

end
