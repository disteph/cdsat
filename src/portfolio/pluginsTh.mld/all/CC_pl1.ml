open Kernel
open Termstructures.Literals
open Top.Specs
open Theories.CC

module Make(DS:GlobalDS) = struct
  open DS

  module Make(K: MyTheory.API with type termdata = Term.datatype
                               and type assign = Assign.t) = struct

    let rec init_rec (module Next : K.SlotMachineCC with type t = K.outputCC)
      = 
      let rec state : (MyTheory.sign,DS.Assign.t) slot_machine =
        (module struct 
           type newoutput = (MyTheory.sign,DS.Assign.t) output
           type tset = DS.Assign.t
           let add = function
             | None -> Output(None,state)
             | Some tset -> 
                match Next.add tset with
                | K.UNSAT msg     -> Output(Some msg, Top.Tools.fail_state)
                | K.SAT(msg,cont) -> Output(Some msg, init_rec cont)
           let normalise _ = failwith "Not a theory with normaliser"
           let clone () = Output(None,state)
           let suicide _ = ()
         end)
      in
      state

  end

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      let module M = Make(K) in
      {
        PluginTh.init = M.init_rec K.init;
        PluginTh.clear = (fun () -> ())
      }
        
end
