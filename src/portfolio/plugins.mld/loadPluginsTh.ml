open Kernel.Theories.Register
       
module Make(K:Kernel.Export.API) = struct

  (* We create a map pluginsTh that maps every (involved) theory handler to (the
      initial state of) a decision procedure for it. *)

  let add_plugin
        (Modules.Module(tag,_) as plugin)
        (plugins_sofar, clear_sofar) =
    let module Pl = PluginsTh.Register.Make(K.WB.DS) in
    let init,clear = Pl.make plugin in
    HandlersMap.add (Handlers.Handler tag) init plugins_sofar,
    (fun () -> clear_sofar (); clear ())

  let pluginsTh,clear =
    List.fold
      add_plugin
      K.th_modules
      (HandlersMap.empty,(fun () -> ()))

end
