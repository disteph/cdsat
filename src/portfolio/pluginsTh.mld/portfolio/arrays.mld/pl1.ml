open Kernel
open Top.Terms
open Theories.Arrays

type sign = MyTheory.sign
type api  = (module MyTheory.API)

let hdl = MyTheory.hdl
  
module Make(W: Writable) = struct
  
  let make (module K : MyTheory.API)
    = {
        PluginTh.init  = K.init;
        PluginTh.clear = (fun () -> ())
      }

end
