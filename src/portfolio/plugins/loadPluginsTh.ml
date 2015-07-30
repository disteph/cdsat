open Kernel
open Top
open Theories_register
open Combo
open Types
open PluginsTh_register

module type GetPlugins = sig
  include Plugin.DataList
  val make :
    (module Specs.GTheoryDSType with type Term.datatype = 't and type TSet.t = 'ts)
    -> ('t,agglo) projList
    -> ('ts -> 'ts slot_machine) HandlersMap.t
end

let make theories : (module GetPlugins) =
  HandlersMap.fold
    (fun (Handlers.Handler sign as hdl) () gp
    -> let module GP = (val gp: GetPlugins) in
       let module PluginTh = (val get_default sign) in
       (module struct
         type agglo = PluginTh.ThDS.t * GP.agglo
         let dataList = ConsData((module PluginTh.ThDS),GP.dataList)
         let make (type t)(type ts) ds (ConsProj(g,projlist)) =
           let module DS = (val ds: Specs.GTheoryDSType with type Term.datatype = t and type TSet.t = ts) in
           let module S = PluginTh.Make(struct include DS let proj = g end) in
           HandlersMap.add hdl S.search (GP.make ds projlist)
       end: GetPlugins)
    )
    theories
    (module struct 
      type agglo = unit
      let dataList = NoData
      let make _ NoProj = HandlersMap.empty
    end:GetPlugins)
