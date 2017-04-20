open Format

open Top
open Basic
open Specs
open Variables

open Termstructures
open Literals
(* open Prop.Interfaces_theory *)

type sign = unit
type ts = TS.t
let ts  = Termstructures.Register.LitF

include Theory.HasNoValues
          
module Make(DS: DSproj with type ts = Literals.TS.t) = struct

  open DS
  type term = Term.datatype
  type assign = Assign.t

  let asL term = let l,ms = snd(Terms.data term) in
                 (* print_endline (Dump.toString (fun p -> p "Lit: %a" LitF.print_in_fmt l)); *)
                 let b,id = LitF.reveal l in
                 let atom = Term.term_of_id id in
                 (* print_endline (Dump.toString (fun p -> p "Term: %a" Term.print_in_fmt atom)); *)
                 b,atom,ms

  (* let makes_sense term = MakesSense.check (snd(snd(Terms.data term))) *)

  module Constraint = Constraint

end

module type API = sig
  type term
  type assign
  (* val init: (sign,assign) slot_machine *)
  (* val clear: unit -> unit *)
end

type ('t,'v,'a) api = (module API with type term = 't
                                   and type assign = 'a)

let make (type t)(type v)(type a)
      ((module DS): (ts,values,t,v,a) dsProj)
    : (t,v,a) api =
  (module Make(DS))



                                                         
                        (*   open DS *)

                        (*   exception FoundIt of Assign.t*Constraint.t*(Assign.t,Constraint.t) stream *)

                        (* (\* We range over atomN, *)
                        (*    alias is the set of atoms that haven't been tried (starts with atomN); *)
                        (*    as soon as we find a solution, we raise an exception *\) *)

                        (*   let aux aset compare cont t atomN sigma =  *)
                        (*     Assign.fold *)
                        (*       (fun a alias ->  *)
                        (*         Dump.print ["FirstOrder",1] (fun p -> p "Unifying literals %a and %a" Term.pp a Term.pp t); *)
                        (*         let newalias = Assign.remove a alias in *)
                        (*         (match asL a, asL t with *)
                        (*         | (b1,l1,_), (b2,l2,_) when compare b1 b2 -> *)
                        (*           (Dump.print ["FirstOrder",1] (fun p -> p "constraint = %a" Constraint.pp sigma); *)
                        (*            Dump.print ["FirstOrder",1] (fun p -> p "Actually unifying atoms %a and %a" Term.pp l1 Term.pp l2); *)
                        (*            match Constraint.unif sigma l1 l2 with *)
                        (*            | Some newsigma ->  *)
                        (*              raise (FoundIt(Assign.add a aset,newsigma, cont newalias)) *)
                        (*            | None -> ()) *)
                        (*         | _ -> ()); *)
                        (*         newalias) *)
                        (*       atomN  *)
                        (*       atomN  *)
                        
                        (*   let rec goal_consistency t atomN sigma =  *)
                        (*   (\* (print_endline (Dump.toString(fun p->p "goal consistency on %a |- %a" Assign.pp atomN))); *\) *)
                        (*   (\* We range over atomN, *)
                        (*      we catch an exception (means success); *)
                        (*      otherwise we finish with NoMore *\) *)
                        (*     try  *)
                        (*       let _ =  *)
                        (*         aux Assign.empty [%eq:bool] (goal_consistency t) t atomN sigma *)
                        (*       in NoMore *)
                        (*     with *)
                        (*       FoundIt(a,newsigma,f) -> Guard(a,newsigma,f) *)

                        (*   let rec consistency atomN sigma = *)
                        (*     try  *)
                        (*       let _ = *)
                        (*         Assign.fold *)
                        (*           (fun t after ->  *)
                        (*             let newafter = Assign.remove t after in *)
                        (*             let aset = Assign.add t Assign.empty in *)
                        (*             let rec g_consistency remaining sigma = *)
                        (*               try  *)
                        (*                 let _ = aux aset (!=) g_consistency t remaining sigma in *)
                        (*                 consistency newafter sigma *)
                        (*               with *)
                        (*                 FoundIt(a,newsigma,f) -> Guard(a,newsigma,f) *)
                        (*             in *)
                        (*             let _ = aux aset (!=) g_consistency t newafter sigma in newafter *)
                        (*           ) *)
                        (*           atomN  *)
                        (*           atomN  *)
                        (*       in *)
                        (*       NoMore *)
                        (*     with *)
                        (*       FoundIt(a,newsigma,f) -> Guard(a,newsigma,f) *)

                        (* end *)
