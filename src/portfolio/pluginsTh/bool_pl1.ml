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
open MyStructures
       
type sign = MyTheory.sign
let hdl = Sig.Bool

(* Needed in our mli *)
            
module ThDS = ThDS

(* We are implementing VSIDS heuristics for choosing decision
literals: we need to keep a score of each lit, we need to update the
scores, and we need to pick the lit with highest score. We use a
patricia tries for this. *)
  
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

let decay = 2.
let factor = decay**100.
                
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
  let since_last = ref 1

  let bump lset =
    scores := LMap.union_poly (fun _ () v -> v +. !bump_value)
                (LMap.map (fun _ () -> !bump_value))
                (fun scores -> scores)
                lset !scores;
    incr since_last;
    bump_value := !bump_value *. decay;
    if !since_last > 100
    then (scores := LMap.map (fun _ score -> score /. factor) !scores;
          bump_value := !bump_value /. factor;
          since_last := 1)
                                   
                                   
  type state = {
      todo      : Term.t Pqueue.t;          (* Set of terms to be passed to kernel *)
      outqueue  : Config.straight Pqueue.t; (* Queue for outgoing messages *)
      finalsay  : (Config.straight list
                   * (sign, TSet.t, unsat) message) option; (* Has the kernel already found a conflict? *)
      propastate: Propa.t;                  (* State of the 2-watched algorithm *)
      already   : (TSet.t*TSet.t) option    (* Last split we have asked for, if any *)
    }

  let rec machine state =
    (module
       struct
          
         type newoutput = (sign,TSet.t) output
         type tset = TSet.t

         let add newlits =

           let state =
             match state.finalsay, newlits with
             | None, Some nl when not(TSet.is_empty nl) ->
                let already =
                  match state.already with
                  | Some(a,b) when TSet.equal a nl || TSet.equal b nl
                    -> None
                  | _ -> state.already
                in
                { state with todo = TSet.fold Pqueue.push nl state.todo;
                             already = already }
             | _    -> state
           in

           match state.finalsay with

           | Some([],msg) ->
              Output(Some msg,fail_state)

           | Some(msg::l,unsat) ->
              let state = { state with finalsay = Some(l,unsat) } in
              Output(Some msg, machine state)

           | None ->
              
              let rec aux state =
                match Pqueue.pop state.todo, Pqueue.pop state.outqueue with

                | Some(t,todo), _ ->
                   Dump.print ["bool_pl1",2] (fun p -> p "Treating: %a" Term.print_in_fmt t);
                   (* A term we have not treated yet! *)
                   (* We start by maybe updating the VSIDS structures*)
                   begin
                     match (proj(Terms.data t)).asclause with (* Let's look at its clausal structure *)
                     | Some lset when LSet.info lset > 1 ->
                        (* Every literal in lset that we have never seen before is given score 1. *)
                        let toadd = LMap.map (fun _ () -> 1.) lset in
                        scores := LMap.union (fun score _ -> score) !scores toadd
                     | _ -> () (* Otherwise nothing to do *)
                   end;
                   let c = Config.Constraint.make t in
                   begin
                     match Propa.treat c state.propastate with
                     | A(list,unsat,term) ->
                        begin match (proj(Terms.data term)).asclause with
                        | None -> failwith "Clause is false, cannot be true"
                        | Some lset -> bump lset
                        end;
                        begin match list with
                        | []     -> Output(Some unsat,fail_state)
                        | msg::l ->
                           let state = { state with finalsay = Some(l,unsat) } in
                           Output(Some msg, machine state)
                        end
                     | F propastate ->
                        (* If its negation-normal form is a conjunction, we unfold it *)
                        begin match Config.unfold t with
                        | Some(Propa(_,Straight tset) as msg) ->
                           aux { state with todo = todo;
                                            outqueue = Pqueue.push msg state.outqueue;
                                            propastate = propastate  }
                        | None ->
                           aux { state with todo = todo;
                                            propastate = propastate  }
                        end
                   end

                | None, Some(msg,outqueue) ->
                   let next = machine { state with outqueue = outqueue } in
                   Output(Some msg, next)

                | None, None ->

                   let msg,propastate = Propa.extract_msg state.propastate in
                   let state = { state with propastate = propastate } in
                   
                   match msg,state.already with

                   | Some(Config.Msg message),_ ->
                      Output(Some message, machine state)
                            
                   | Some(Config.SplitBut lset), None ->
                      let remaining =
                        LMap.diff_poly
                          (fun _ _ _ -> LMap.empty)
                          !scores
                          lset
                      in
                      Dump.print ["bool_pl1",2]
                        (fun p->p "Choosing among %a"
                                  (LMap.print_in_fmt
                                     (fun fmt (l,_) -> LitF.print_in_fmt fmt l)
                                  )
                                  remaining
                        );
                      begin match LMap.info remaining with
                      | Some(lit,_) ->
                         let msg = Config.split lit in
                         let Propa(_,Both(t1,t2)) = msg in
                         Dump.print ["bool_pl1",2]
                           (fun p->p "Splitting on literal %a: %a"
                                     LitF.print_in_fmt
                                     lit
                                     (Messages.print_msg_in_fmt TSet.print_in_fmt)
                                     msg
                           );
                         let state = { state with already = Some(t1,t2) }
                         in
                         Output(Some msg, machine state)
                      | None ->
                         Output(None, machine state)
                      end
                   | _,Some(t1,t2) ->
                      Dump.print ["bool_pl1",2]
                        (fun p->p "Waiting to know about (%a, %a)"
                                  TSet.print_in_fmt t1
                                  TSet.print_in_fmt t2
                        );
                      Output(None, machine state)
                   | _,_ ->
                      Output(None, machine state)

              in
              aux state

         let normalise = (fun _ -> failwith "Not a theory with normaliser")
                           
         let clone = (fun () -> Output(None, machine { state with already = None }))
                       
       end : SlotMachine with type newoutput = (sign,TSet.t) output and type tset = TSet.t)
      
  let init = machine
               { todo = Pqueue.empty;
                 outqueue = Pqueue.empty;
                 finalsay = None;
                 propastate = Propa.init;
                 already  = None }

end
