open Kernel.Theories.Arrays

include PluginTh.Type with type sign := MyTheory.sign and type ('t,'v,'a) api := ('t,'v,'a) MyTheory.api
