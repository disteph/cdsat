open Kernel.Theories.Bitvectors

include PluginTh.Type with type sign := MyTheory.sign and type ('t,'v,'a,'s) api := ('t,'v,'a,'s) MyTheory.api
