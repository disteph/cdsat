open Kernel
open Top
open Specs
open Sassigns
open Theories.IfThenElse

type sign = MyTheory.sign

module Make(WB:Export.WhiteBoard) = struct
  open WB
  open DS

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      let rec get state =
        match K.what_now state with
        | Some(K.Sat msg), state ->   Msg msg, machine state
        | Some(K.Propa msg), state -> Msg msg, machine state
        | None, state ->
           let tset = K.wondering state in
           let term = K.TSet.choose tset in
           Try(boolassign term), machine state
        and machine state =
          Specs.SlotMachine {
              add = (function
                | None -> get state
                | Some sassign -> get(K.add sassign state));
              
              clone   = (fun () -> machine state);
              suicide = (fun _ -> ())
            }
      in
      
      {
        PluginTh.init = machine K.init;
        PluginTh.clear = fun ()->()
      }

end
