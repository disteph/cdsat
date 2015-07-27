open Kernel
open Top
open Messages
open Register
open Combo
open Types

let make theories : (module Plugin.Type) =
  (module struct

    include (val LoadPluginsTh.make theories)

    module Strategy(WB: sig
      include Interfaces.WhiteBoard
      val projList: (DS.Term.datatype,agglo) projList
    end) = struct
        
      let solve tset =
        let m = make (module WB.DS) WB.projList tset in
        let rec aux (SM(ans,f)) = match ans with
          | None -> aux (f None)
          | Some(thans) -> match thans with
            | ThAns(_,ThProvable tset)    -> WB.check(WB.PlProvable(thans))
            | ThAns(_,ThNotProvable tset) -> WB.check(WB.PlNotProvable(tset,[thans]))
            | _ -> failwith "Don't know what to do yet"
        in
        HandlersMap.fold
          (fun _ sm _ -> aux sm)
          m
          (WB.check(WB.PlNotProvable(tset,[])))
          
    end

  end)
