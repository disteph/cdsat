open Kernel.Theories.Bool

include PluginTh.Type with type sign := MyTheory.sign and type ('t,'v,'a) api := ('t,'v,'a) MyTheory.api
