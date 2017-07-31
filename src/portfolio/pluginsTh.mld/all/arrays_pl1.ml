open Kernel
open Top.Specs
open Theories.Arrays

type sign = MyTheory.sign

module Make(WB:Export.WhiteBoard) = struct
  open WB
  open DS

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      {
        PluginTh.init = K.init;
        PluginTh.clear = K.clear
      }

end
