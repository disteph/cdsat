(************************************)
(* Main entry point for Psyche runs *)
(************************************)

val init :
      (module Theories.Prop.APIplugin.PlugDSType with type UASet.t='uaset
                                                  and type UF.t = 'uf
                                                  and type UFSet.t='ufset)
      -> (module Parsers.Parser.Type)
      -> string
      -> (module Export.API with type u = 'uaset*'uf*'ufset)
