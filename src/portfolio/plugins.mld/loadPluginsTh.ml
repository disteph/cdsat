open Kernel.Theories.Register

module Make(K:Kernel.Export.API) = struct

  (* We create a map pluginsTh that maps every (involved) theory handler to (the
      initial state of) a decision procedure for it. *)

  let get_machine (Modules.Module(tag,api)) = failwith "TO IMPLEMENT"

  let add_plugin ((Modules.Module(tag,_)) as m) =
    HandlersMap.add (Handlers.Handler tag) (get_machine m)

  let pluginsTh = List.fold add_plugin K.th_modules HandlersMap.empty

  let clear () = ()

end
