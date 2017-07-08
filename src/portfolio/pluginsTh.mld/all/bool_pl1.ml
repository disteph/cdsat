open Kernel
open Top.Specs
open Theories.Bool

type sign = MyTheory.sign

module Make(DS:GlobalDS) = struct

  open DS

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      {
        PluginTh.init = K.init;
        PluginTh.clear = K.clear
      }

end
