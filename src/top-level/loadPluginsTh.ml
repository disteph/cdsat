open Kernel
open Top
open Specs
open Theories_register
open Combo

type _ sslot_machine = Signed: 'sign Sig.t * ('sign,'ts) slot_machine -> 'ts sslot_machine

module type GetPlugins = sig
  include Plugin.DataList
  val make :
    (module Specs.GTheoryDSType with type Term.datatype = 't and type TSet.t = 'ts)
    -> ('t,agglo) projList
    -> ('ts sslot_machine) HandlersMap.t * (unit->unit)
end

let make theories : (module GetPlugins) =
  HandlersMap.fold
    (fun (Handlers.Handler sign as hdl) () (module GP : GetPlugins)
     -> let (module PluginTh: PluginsTh.PluginTh.Type) = PluginsTh.Register.get_default sign in
        (module struct

           type agglo = PluginTh.ThDS.t * GP.agglo

           let dataList = ConsData((module PluginTh.ThDS),GP.dataList)

           let make (type t)(type ts) ds (ConsProj(g,projlist)) =
             let module DS = (val ds: Specs.GTheoryDSType with type Term.datatype = t and type TSet.t = ts) in
             let module S = PluginTh.Make(struct include DS let proj = g end) in
             let hdlmap,clear = GP.make ds projlist in
             HandlersMap.add hdl (Signed(PluginTh.hdl,S.init)) hdlmap,
             (fun () -> clear();S.clear())
               
               end: GetPlugins)
    )
    theories
    (module struct 
       type agglo = unit
       let dataList = NoData
       let make _ NoProj = HandlersMap.empty,(fun()->())
                                               end:GetPlugins)
