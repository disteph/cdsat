open Kernel
open Export
open Top.Specs
open Theories.Arrays

type sign = MyTheory.sign

module Make(DS: GlobalImplem) = struct
  open DS

  let make (k: (Term.datatype,Value.t,Assign.t,TSet.t) MyTheory.api)
    = let (module K) = k in
      {
        PluginTh.init = K.init;
        PluginTh.clear = K.clear
      }

end
