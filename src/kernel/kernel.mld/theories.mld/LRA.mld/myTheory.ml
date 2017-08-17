open General
open Top
open Basic
open Messages
open Specs
open Sassigns
       
open Termstructures.Rationals
       
type sign = unit

(* We are not using values *)
include Theory.HasValues(Qhashed)

(* Our alternative term representation is the set of free variables *)
type ts = TS.t
let ts = Termstructures.Register.Rationals

module Make(DS: DSproj with type ts = ts) = struct

  open DS
  type datatypes = Term.datatype*Value.t*Assign.t*TSet.t

  let add_myvars term myvars =
    let aux var _ = var |> IntSort.reveal |> fun (i,_) -> TSet.add (Term.term_of_id i) in
    TS.(VarMap.fold aux (DS.proj(Terms.data term)).coeffs myvars)

  type state = { assign : Assign.t;
                 sharing: TSet.t;
                 myvars : TSet.t Lazy.t }
                                           
  let rec machine state =
    let add = function
      | None ->
         Print.print ["kernel.LRA",2] (fun p ->
             p "kernel.LRA receiving None");
         Silence, machine state
      | Some sassign ->
         Print.print ["kernel.LRA",2] (fun p ->
             p "kernel.LRA receiving Some(%a)" pp_sassign sassign);
         let assign = Assign.add sassign state.assign in
         let SAssign(term,_) = sassign in
         let myvars = lazy(add_myvars term (Lazy.force state.myvars)) in
         let state = { state with assign; myvars } in
         Msg(sat () state.assign ~sharing:state.sharing ~myvars ),
         machine state
    in
    let share tset = 
      Print.print ["kernel.LRA",2] (fun p ->
          p "kernel.LRA notified than %a are shared" TSet.pp tset);
      let sharing = TSet.union tset state.sharing in
      let myvars = lazy(TSet.fold add_myvars tset (Lazy.force state.myvars)) in
      let state = { state with sharing; myvars } in
      Msg(sat () state.assign ~sharing ~myvars),
      machine state
    in
    let clone () = machine state in
    let suicide _ = () in
    Specs.SlotMachine { add; share; clone; suicide }

  let init = machine { assign=Assign.empty; sharing=TSet.empty; myvars=lazy TSet.empty }
  let clear () = ()
                   
end

module type API = sig
  type datatypes
  val init: (sign,datatypes) slot_machine
  val clear: unit -> unit
end

type ('t,'v,'a,'s) api = (module API with type datatypes = 't*'v*'a*'s)

let make (type t v a s)
      ((module DS): (ts,values,t,v,a,s) dsProj)
    : (t,v,a,s) api =
  (module Make(DS))
