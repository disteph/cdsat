open Kernel.Theories.Arrays

include PluginTh.Type with type sign = MyTheory.sign
                       and type api  = (module MyTheory.API)
