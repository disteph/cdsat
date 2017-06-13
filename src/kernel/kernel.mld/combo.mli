(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Interfaces_basic
open Basic
open Variables
open Theories
open Register
open Specs

(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.DataType
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)

val make :
  Terms.TermB.t list
  -> bool option
  -> unit HandlersMap.t
  -> (module Prop.APIplugin.PlugDSType
             with type UASet.t = 'uaset
              and type UF.t    = 'uf
              and type UFSet.t = 'ufset)
  -> (module Export.API with type uaset = 'uaset
                         and type uf    = 'uf
                         and type ufset = 'ufset)
