open Kernel
open Termstructures.Literals
open Termstructures.Clauses
open Top.Specs
open Top.Messages

open General
open Patricia
open Patricia_interfaces
open SetConstructions
open Sums

open Tools.PluginsTh

open Theories.Bool

type sign = MyTheory.sign

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
          | Some(_,s1),Some(_,s2)->
             if [%ord:float] s1 s2 < 0 then x2 else x1
        )
    }
  let treeHCons  = None (* Some(LitF.id,int_of_float,(=)) *)
end

module LMap = PATMap.Make(DMap)(I)

let decay = 3.
let factor = decay**1000.
                      
module Make(DS:GlobalDS) = struct

  open DS

  module Make(K: API.Type with type term = DS.Term.t
                           and type assign = DS.Assign.t) = struct
           
    module Config = struct
      include K
      module Var = LitF
      let howmany = 2
      let pick_another _ (c :Constraint.t) i (varlist :LitF.t list)
          : LitF.t list option =

        match Constraint.verysimpl c with

        (* If clause not already true *)
        | Sums.Case1 set ->
           Tools.PluginsTh.TwoWatchedLits.pick_another_make
             ~is_empty:LSet.is_empty
             ~mem:LSet.mem
             ~next:LSet.next
             ~remove:LSet.remove
             set i varlist

        (* If clause is already true: *)
        | Sums.Case2 _ -> None

    end

    module Propa = Propagate.Make(Config)

    let scores = ref LMap.empty
    let bump_value = ref 1.
    let since_last = ref 1

    let bump lset =
      scores := LMap.union_poly (fun _ () v -> v +. !bump_value)
                  (* (LMap.map (fun _ () -> !bump_value)) *)
                  (fun _ -> LMap.empty)
                  (fun scores -> scores)
                  lset !scores;
      Dump.print ["bool_pl1",2] (fun p ->
          p "Cardinal of !scores: %i" (LMap.cardinal !scores));
      incr since_last;
      bump_value := !bump_value *. decay;
      if !since_last > 1000
      then (scores := LMap.map (fun _ score -> score /. factor) !scores;
            bump_value := !bump_value /. factor;
            since_last := 1)
             
             
    type state = {
        todo      : Term.t Pqueue.t; (* Set of terms to be passed to kernel *)
        finalsay  : (Config.straight list
                     * (K.sign, Assign.t, unsat) message) option;
        (* Has the kernel already found a conflict? *)
        propastate: Propa.t;                  (* State of the 2-watched algorithm *)
        already   : (Assign.t*Assign.t) option    (* Last split we have asked for, if any *)
      }

    let rec machine state
            : (module SlotMachine with type newoutput = (K.sign,Assign.t) output
                                   and type tset = Assign.t) =
      (module struct
         
         type newoutput = (K.sign,Assign.t) output
         type tset = Assign.t

         let add newlits =

           let state =
             match state.finalsay, newlits with
             | None, Some nl when not(Assign.is_empty nl) ->
                let already =
                  match state.already with
                  | Some(a,b) when Assign.equal a nl || Assign.equal b nl
                    -> None
                  | _ -> state.already
                in
                { state with todo = Assign.fold Pqueue.push nl state.todo;
                             already = already }
             | _    -> state
           in

           match state.finalsay with

           | Some([],msg) ->
              Output(Some msg,Top.Tools.fail_state)

           | Some(msg::l,unsat) ->
              let state = { state with finalsay = Some(l,unsat) } in
              Output(Some msg, machine state)

           | None ->
              
              let rec aux state =
                match Pqueue.pop state.todo with

                | Some(t,todo) ->
                   Dump.print ["bool_pl1",2] (fun p ->
                       p "Treating: %a" Term.pp t);
                   (* A term we have not treated yet! *)
                   (* We start by maybe updating the VSIDS structures*)
                   begin
                     (* Let's look at its clausal structure *)
                     match (K.proj t).asclause with
                     | Some lset ->
                        Dump.print ["bool_pl1",2] (fun p ->
                            p "Putting scores for %a"
                              (LSet.print_in_fmt LitF.print_in_fmt)
                              lset);
                        (* Every literal in lset that we have never seen 
                           before is given score !bump_value. *)
                        scores := LMap.union_poly
                                    (fun _ () score -> score)
                                    (LMap.map (fun _ () -> !bump_value))
                                    (fun scores -> scores)
                                    lset
                                    !scores;
                        Dump.print ["bool_pl1",2] (fun p ->
                            p "Cardinal of !scores: %i" (LMap.cardinal !scores));
                        Dump.print ["bool_pl1",3] (fun p ->
                            p "All scored lits\n%a"
                              (LMap.print_in_fmt
                                 (fun fmt (l,_) -> LitF.print_in_fmt fmt l)
                              )
                              !scores
                          )

                     | None ->  (* Otherwise nothing to do *)
                        Dump.print ["bool_pl1",2] (fun p ->
                            p "Term %a is already true" Term.pp t);
                   end;
                   let c = Config.Constraint.make t in
                   let propastate =
                     match Config.Constraint.verysimpl c with
                     | Case1 lset when LSet.info lset > 1 ->
                        Propa.add_constraint c state.propastate
                     | _ -> state.propastate
                   in
                   begin
                     match Propa.fix (Config.fix t) propastate with
                     (* match Propa.fix (fun fixed -> Config.Meh fixed) propastate with *)
                     | Case1([],unsat)     ->
                        Output(Some unsat,Top.Tools.fail_state)
                     | Case1(msg::l,unsat) ->
                        let state = { state with finalsay = Some(l,unsat) } in
                        Output(Some msg, machine state)
                     | Case2 propastate    ->
                        aux { state with todo = todo;
                                         propastate = propastate  }
                   end

                | None ->

                   let msg,propastate = Propa.extract_msg state.propastate in
                   let state = { state with propastate = propastate } in
                   
                   match msg,state.already with

                   | Some(Config.Msg message),_ ->
                      Dump.print ["bool_pl1",1] (fun p ->
                          p "bool_pl1: Transmitting msg %a"
                            (print_msg_in_fmt Assign.pp)
                            message
                        );
                      Output(Some message, machine state)
                            
                   | Some(Config.SplitBut lset), None ->
                      let remaining =
                        LMap.diff_poly
                          (fun _ _ _ -> LMap.empty)
                          !scores
                          lset
                      in
                      Dump.print ["bool_pl1",3] (fun p ->
                          p "All scored lits\n%a"
                            (LMap.print_in_fmt
                               (fun fmt (l,_) -> LitF.print_in_fmt fmt l)
                            )
                            !scores );
                      Dump.print ["bool_pl1",2] (fun p ->
                          p "Choosing among %a"
                            (LMap.print_in_fmt
                               (fun fmt (l,_) -> LitF.print_in_fmt fmt l)
                            )
                            remaining );
                      begin match LMap.info remaining with
                      | Some(lit,_) ->
                         let msg = Config.split lit in
                         let Propa(_,Both(t1,t2)) = msg in
                         Dump.print ["bool_pl1",2] (fun p ->
                             p "Splitting on literal %a: %a"
                               (LitF.print_in_fmt ~print_atom:Term.print_of_id)
                               lit
                               (print_msg_in_fmt Assign.pp)
                               msg     );
                         let state = { state with already = Some(t1,t2) } in
                         Output(Some msg, machine state)

                      | None -> failwith "Bool_pl1: No lit to split on!!!"
                      end

                   | _,Some(t1,t2) ->
                      Dump.print ["bool_pl1",2]
                        (fun p->p "Waiting to know about (%a, %a)"
                                  Assign.pp t1
                                  Assign.pp t2 );
                      Output(None, machine state)

                   | None,None -> failwith "Bool_pl1: should not happen"
                                           

              in
              aux state

         let normalise = (fun _ -> failwith "Not a theory with normaliser")
                           
         let clone = (fun () -> Output(None, machine { state with already = None }))
                       
         let suicide (Propa(tset,Unsat)) =
           let aux term lset =
             LSet.add ((K.proj term).aslit) lset
           in
           let lset = Assign.fold aux tset LSet.empty in
           bump lset

       end)
        
    let init = machine
                 { todo = Pqueue.empty();
                   finalsay = None;
                   propastate = Propa.init;
                   already  = None }

    let clear() = LMap.clear();
                  scores := LMap.empty;
                  bump_value := 1. ;
                  since_last := 1 ;
                  Config.clear()


  end

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      let module M = Make(K) in
      {
        PluginTh.init = M.init;
        PluginTh.clear = M.clear
      }

end