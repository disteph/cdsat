(* This is the register of all generic plugins in Psyche *)

open Kernel
open Export
open Top.Specs
open Theories.Register.Modules
       
module Make(DS: GlobalImplem) = struct
  open DS

  let make (Module(tag,k): (Term.datatype*Value.t*Assign.t*TSet.t) t) =
    let aux make = 
      let open PluginTh in
      let o = make k in
      Signed(tag,o.init),
      o.clear
    in
    let open Theories.Register.Tags in
      match tag with

      | Bool  ->
         let module M = Bool.Pl1.Make(DS) in
         aux M.make

      (* | CC    -> *)
      (*    let module M = CC_pl1.Make(WB) in *)
      (*    aux M.make *)

      | Arrays->
         let module M = Arrays.Pl1.Make(DS) in
         aux M.make

      | LRA   ->
         let module M = LRA.Pl1.Make(DS) in
         aux M.make

      | IfThenElse ->
         let module M = IfThenElse.Pl1.Make(DS) in
         aux M.make

end
