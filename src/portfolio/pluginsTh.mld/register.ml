open General.Hashtbl_hetero

open Kernel
open Top.Terms
open Theories.Theory
open Theories.Register.Modules
open PluginTh

let all_pluginTh : (module Type) list =
  [ (module Bool.Pl1); 
    (module Arrays.Pl1);
    (module LRA.Pl1);
    (module IfThenElse.Pl1);
    (module Bitvectors.Pl1) ]
  
module Buildable = struct
  type ('a,_) t = Buildable : ('api -> 'sign pluginTh) -> ('sign*'api,_) t [@@unboxed]
end

module PluginMap = MakeS(Tags)(Buildable)

exception NoPluginTh of Handlers.t

module Make(W: Writable) = struct

  let pluginMap = PluginMap.create 17

  let add2map (module Pl:Type) =
    let module PlBuilt = Pl.Make(W) in
    PluginMap.add pluginMap Pl.hdl (Buildable PlBuilt.make)

  let () = List.iter add2map all_pluginTh

  let make (Module(hdl,k)) =
    if PluginMap.mem pluginMap hdl
    then
      let Buildable.Buildable make = PluginMap.find pluginMap hdl in
      let o = make k in
      Signed(hdl,o.init), o.clear
    else
      raise (NoPluginTh(Handlers.Handler hdl))
end
