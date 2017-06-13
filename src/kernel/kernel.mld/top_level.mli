(************************************)
(* Main entry point for Psyche runs *)
(************************************)

val init :
      (module Theories.Prop.APIplugin.PlugDSType with type UASet.t='uaset
                                                  and type UF.t = 'uf
                                                  and type UFSet.t='ufset)
      -> parser:string
      -> ?withtheories:string list option
      -> ?withouttheories:string list option
      -> ?disableProp:bool
      -> string
      -> (module Export.API with type uaset = 'uaset
                             and type uf    = 'uf
                             and type ufset = 'ufset)
