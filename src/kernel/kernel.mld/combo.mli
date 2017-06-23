(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Theories
open Register

val make :
  unit HandlersMap.t
  -> (module Prop.APIplugin.PlugDSType
             with type UASet.t = 'uaset
              and type UF.t    = 'uf
              and type UFSet.t = 'ufset)
  -> (module Export.API with type uaset = 'uaset
                         and type uf    = 'uf
                         and type ufset = 'ufset)
