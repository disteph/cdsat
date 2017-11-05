(***********************************)
(* Main entry point for CDSAT runs *)
(***********************************)

open General.Sums
open PluginsTh.PluginTh
open Kernel.Top.Messages
   
module type Fix = sig
  include Kernel.Export.API
  val solve : WB.DS.Assign.t
              -> (Kernel.Top.Messages.unsat WB.t, WB.sat_ans) General.Sums.sum
  val clear : unit -> unit
end
                
let fix : (module Fix) =
  let theories =
    let open Kernel.Theories.Register in
    HandlersMap.singleton (Handlers.Handler Tags.Bool) ()
  in
  let (module K) = Kernel.Combo.make theories in
  let module Input = struct
      include K

      open Kernel.Theories.Register
         
      module Plugin = PluginsTh.Register.Make(WB.DS)

      let add_plugin
            (Modules.Module(tag,_) as plugin)
            (plugins_sofar, clear_sofar)
        =
        let init,clear = Plugin.make plugin in
        init::plugins_sofar,
        (fun () -> clear_sofar (); clear ())

      let pluginsTh, clear =
        List.fold
          add_plugin
          K.th_modules
          ([],(fun () -> ()))
    end
  in

  let (module Pl)  = Plugins.Register.get "" in
  (module struct
     include K
     include Pl.Make(Input)
               end: Fix)
  
let query problem0 =
  let (module Fix) = fix in
  let open Fix in
  let problem = failwith "TODO" in
  match solve problem with
  | Case1(WB.WB(_,Propa(core,Unsat))) when WB.DS.Assign.subset core problem
    -> failwith "TODO"
  | Case2(WB.Done(assign,sharing)) when WB.DS.Assign.subset problem assign
    -> failwith "TODO"
  | _ -> failwith "Not answering problem"

