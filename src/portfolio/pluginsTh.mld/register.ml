(* This is the register of all generic plugins in Psyche *)

open Kernel
open Top.Specs
open Theories.Register
       
module Make(DS: GlobalDS) = struct
  open DS

  let make (Modules.Module(tag,k): (Term.datatype*Value.t*Assign.t) Modules.t)
    =
    let aux make = 
      let open PluginTh in
      let o = make k in
      Signed(Sig.Sig tag,o.init),
      o.clear
    in
    let open Tags in
      match tag with
      | Empty ->
         let module M = Empty_pl1.Make(DS) in         
         aux M.make

      | Bool  ->
         let module M = Bool_pl1.Make(DS) in
         aux M.make

      | CC    ->
         let module M = CC_pl1.Make(DS) in
         aux M.make

      | Arrays->
         let module M = Arrays_pl1.Make(DS) in
         aux M.make

      | LRA   ->
         let module M = LRA_pl1.Make(DS) in
         aux M.make

      | IfThenElse ->
         let module M = IfThenElse_pl1.Make(DS) in
         aux M.make

      | FirstOrder -> failwith "No plugin for FirstOrder"

end
