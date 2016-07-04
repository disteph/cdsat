open Kernel
open Top
open Specs
open Messages

open Prop.Literals
open Theories_register

open General
open Patricia
open Patricia_interfaces
open SetConstructions
open Sums

open PluginsTh_tools

open Bool

type sign = MyTheory.sign
let hdl = Sig.Bool

(* Needed in our mli *)
            
module ThDS = MyStructures.ThDS

(* We are implementing VSIDS heuristics for choosing decision
literals: we need to keep a score of each lit, we need to update the
scores, and we need to pick the lit with highest score. We use a
patricia tries for this. *)
                
module I = TypesFromHConsed(LitF)
  
module DMap = struct
  type keys      = LitF.t
  let kcompare   = LitF.compare
  type values    = float
  type infos     = (LitF.t*float) option
  let info_build = {
      empty_info  = None;
      leaf_info   =
        (fun lit score -> Some(lit,score));
      branch_info =
        (fun x1 x2 ->
          match x1,x2 with
          | None,_ -> failwith "Bad1"
          | _,None -> failwith "Bad2"
          | Some(l1,s1),Some(l2,s2)->
             if s1 < s2 then x2 else x1
        )
    }
  let treeHCons  = Some(LitF.id,int_of_float,(=))
end

module LMap = PATMap.Make(DMap)(I)

let decay = 1.5
                
module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> ThDS.t
end) =
struct

  open DS

  module Config = struct
    include MyTheory.Make(DS)
    module Var = LitF
  end

  module Propa = Propagate.Make(Config)

  let scores = ref LMap.empty
  let bump_value = ref 1.

  let bump lset =
    begin match LMap.info !scores with
    | Some(_,max_score) when max_score > max_float *. 0.5
      -> scores := LMap.map (fun _ score -> score *. 0.0000000001 ) !scores
    | _ -> ()
    end;
    scores := LMap.diff_poly
                (fun lit score () -> LMap.singleton lit (score +. !bump_value))
                !scores
                lset;
    bump_value := !bump_value *. decay
                                   
                                   
  type state = {
      treated: TSet.t;              (* Set of terms received so far, not really treated, actually *)
      todo   : Term.t Pqueue.t;     (* Set of terms to be passed to kernel *)
      outqueue : Config.straight Pqueue.t; (* Queue for outgoing messages *)
      finalsay: Config.stop option; (* Has the kernel already found a conflict? *)
      propastate: Propa.t           (* State of the 2-watched algorithm *)
    }

  let rec machine state =
    (module
       struct
          
         type newoutput = (sign,TSet.t) output
         type tset = TSet.t

         let treated() = state.treated

           
         let add newlits =

           let state =
             match state.finalsay, newlits with
             | Some([],msg,_), _ -> state
             | _, Some nl when not(TSet.is_empty nl) ->
                {
                  state with
                  treated = TSet.union state.treated nl;
                  todo    = TSet.fold Pqueue.push nl state.todo;
                }
             | _    -> state
           in

           match state.finalsay with

           | Some([],msg,_) ->
              Output(Some msg,fail_state)

           | Some(msg::l,unsat,term) ->
              let state = { state with finalsay = Some(l,unsat,term) } in
              Output(Some msg, machine state)

           | None ->
              
              let rec aux state =
                match Pqueue.pop state.todo, Pqueue.pop state.outqueue with

                | Some(t,todo), _ ->
                   Dump.print ["bool_pl1",2] (fun p -> p "Treating: %a" Term.print_in_fmt t);
                   (* A term we have not treated yet! *)
                   (* We start by maybe updating the VSIDS structures*)
                   begin
                     match proj(Terms.data t) with (* Let's look at its clausal structure *)
                     | None,_ -> ()      (* It's the trivially true clause, nothing to do *)
                     | Some lset,_ ->
                        (* Every literal in lset that we have never seen before is given score 1. *)
                        let toadd = LMap.map (fun _ () -> 1.) lset in
                        scores := LMap.union (fun score _ -> score) !scores toadd
                   end;
                   let c = Config.Constraint.make t in
                   begin
                     match Propa.treat c state.propastate with
                     | A(list,unsat,term) ->
                        begin match proj(Terms.data term) with
                        | None,_ -> failwith "Clause is false, cannot be true"
                        | Some lset,_ -> bump lset
                        end;
                        begin match list with
                        | [] -> Output(Some unsat,fail_state)
                        | msg::l ->
                           let state = {state with finalsay = Some(l,unsat,term) } in
                           Output(Some msg, machine state)
                        end
                     | F propastate ->
                        begin match Config.unfold t with
                        | Some(ThStraight(tset,_) as msg) ->
                           aux {state with
                                 todo = (* TSet.fold Pqueue.push tset *) todo;
                                 outqueue = Pqueue.push msg state.outqueue;
                                 propastate = propastate
                               }
                        | None ->
                           aux {state with
                                 todo = todo;
                                 propastate = propastate  }
                        end
                   end

                | None, Some(msg,outqueue) ->
                   let next = machine { state with outqueue = outqueue } in
                   Output(Some msg, next)

                | None, None ->

                   let msg,propastate = Propa.extract_msg state.propastate in
                   let next = machine { state with propastate = propastate } in
                   
                   match msg with

                   | Some(Config.Msg message) ->
                      Output(Some message, next)
                            
                   | Some(Config.SplitBut lset) ->

                      let remaining =
                        LMap.diff_poly
                          (fun _ _ _ -> LMap.empty)
                          !scores
                          lset
                      in
                      Dump.print ["bool_pl1",2]
                        (fun p->p "Choosing among %a"
                                  (LMap.print_in_fmt
                                     None
                                     (fun fmt (l,_) -> LitF.print_in_fmt fmt l)
                                  )
                                  remaining
                        );
                      begin match LMap.info remaining with
                      | Some(lit,_) ->
                         let msg = Config.split lit in
                         Dump.print ["bool_pl1",2]
                           (fun p->p "Splitting on literal %a: %a"
                                     LitF.print_in_fmt
                                     lit
                                     (Messages.print_msg_in_fmt TSet.print_in_fmt)
                                     msg
                           );
                         Output(Some msg, next)
                      | None ->
                         Output(None, next)
                      end
                   | None -> Output(None, next)

              in
              aux state

         let normalise = (fun _ -> failwith "Not a theory with normaliser")
                           
         let clone = (fun () -> Output(None, machine state))
                       
       end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)
      
  let init = machine
               { treated = TSet.empty;
                 todo = Pqueue.empty;
                 outqueue = Pqueue.empty;
                 finalsay = None;
                 propastate = Propa.init }

end
