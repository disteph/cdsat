open Kernel.Theories.Bool

include PluginTh.Type with type sign = MyTheory.sign
                       and type api  = (module API.API with type sign = MyTheory.sign)
