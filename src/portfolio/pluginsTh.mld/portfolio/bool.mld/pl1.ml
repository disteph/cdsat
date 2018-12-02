open General
open Sums
open Patricia
open Patricia_tools
       
open Kernel
open Top.Terms
open Top.Sassigns
open Top.Messages
open Termstructures.Clauses

open Theories.Theory
open Theories.Bool

open PluginTh
open Tools
       
type sign = MyTheory.sign
type api  = (module API.API with type sign = MyTheory.sign)

let hdl = MyTheory.hdl

module Make(W:Writable) = struct

  module Make(K: API.API with type sign = MyTheory.sign) = struct
                                                      
    (* We are implementing VSIDS heuristics for choosing decisions:
       we need to keep a score of each assignment, we need to update the scores,
       and we need to pick the lit with highest score.
       We use a patricia tries for this. *)
                                             
    module ArgMap = struct
      include SAssign
      include TypesFromHConsed(SAssign)
      type values    = float
      let pp_binding fmt (l,f) = Format.fprintf fmt "%a:%f" SAssign.pp l f
      type infos     = (SAssign.t*float) option*int
      let info_build = {
          empty_info  = None,0 ;
          leaf_info   =
            (fun lit score -> Some(lit,score),1);
          branch_info =
            (fun (x1,c1) (x2,c2) ->
              match x1,x2 with
              | None,_ -> failwith "Bad1"
              | _,None -> failwith "Bad2"
              | Some(_,s1),Some(_,s2)->
                (if [%ord:Floathashed.t] s1 s2 < 0 then x2 else x1),
                c1+c2
            )
        }
    end

    module BaMap = struct
      include Map.MakeNH(ArgMap)
      let pp = print_in_fmt ArgMap.pp_binding
    end
                     
    let decay = !PFlags.bool_decay
    let factor = decay**1000.
    let scores = ref BaMap.empty
    let bump_value = ref 1.
    let since_last = ref 1
                       
    (* Configuration for the 2-watched literals module *)
    module Config = struct
      module M = TwoWatchedLits.StdMonad(struct type t = K.Model.t end)
      module Constraint = K.Constraint
      module Var = BAssign
      let simplify = Constraint.simplify
      let pick_another c i _ _ =
        match Constraint.simpl c with
        | Some(_,watchable) -> watchable
        | None -> []
    end

    (* 2-watched literals module *)
    module WL = TwoWatchedLits.Make(Config)

    type state = {
        kernel : K.state;      (* The state of the kernel *)
        fixed  : K.Model.t;    (* Our Boolean model *)
        watched: WL.t;         (* The state of our watched literals *)
        propas : (K.sign,straight) message Pqueue.t; (* The propa messages we need to send*)
        undetermined : Assign.t; (* The set of assignments we could fix *)
        silent : bool          (* Whether we have already sent an unsat message *)
      }
                   

    (* We are asked whether we have something to say *)
    let rec speak machine state =
      (* First, we look at whether there are some propagation messages to send *)
      match Pqueue.pop state.propas with
      | Some(msg,propas) -> (* ...in which case we send a propagation message *)
         Msg msg, machine { state with propas }
      | None -> 
         (* With no propagation message we ask the watched literals
            if a clause is weird *)
         let output,watched = WL.next ~howmany:2 state.watched state.fixed in
         match output with
         | Case1 newlits ->
            (* All clauses seem fine. Maybe the problem is sat ? *)
            Print.print ["bool",4] (fun p -> p "bool: Watched literals are done");
            (* Now we ask the kernel whether it still has any constraints to satisfy *)
            let kernel,todo = K.sat state.fixed state.kernel in
            let state = { state with watched; kernel } in
            begin match todo with
            | Some msg ->
               (* Kernel says all clauses are satisfied and gave us the message to send *)
               Msg msg, machine state

            | None ->
              (* Some clauses still need to be satisfied somehow. It's time to split. *)
               Silence, machine state
            end

         | Case2(c,_) ->
            (* Watched literals have found a weird clause.
               Let's see what the kernel can make of it. *)
            let open K in
            Print.print ["bool",4] (fun p ->
                p "bool: Watched lits have found weird clause %a" Constraint.pp c);
            match infer c with
            | Falsified msg ->
               (* Clause is false and kernel gave us the unsat message to send. *)
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" pp_message msg);
               Msg msg, machine { state with watched; silent = true }
            | Unit msg ->
               (* Clause is unit and kernel gave us the propagation message to send. *)
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" pp_message msg);
               Msg msg, machine { state with watched }
            | Satisfied ->
               (* Clause is satisfied. We look for something else to say *)
               Print.print ["bool",4] (fun p ->
                   p "bool: kernel says %a is satisfied" Constraint.pp c);
               speak machine { state with watched }
            | ToWatch _ -> failwith "Watched Literals got it wrong"

    
    (* This is the main loop. *)                   
    let rec machine state =

      let add =
        ((* If we have already sent an unsat message, we shut up. *)
         if state.silent then fun _ -> Silence, machine state
         else function
           | None -> (* We haven't been given any new information.
                            Let's see if we have something to say *)
              Print.print ["bool",2] (fun p -> p "bool receiving useless stuff");
              speak machine state

           | Some a ->
              Print.print ["bool",2] (fun p ->
                  p "bool receiving Some(%a)" SAssign.pp a);
              (* We ask the kernel to record the new assignment *)
              let kernel, recorded = K.add a state.kernel in
              match SAssign.reveal a with

              | SAssign(_,Top.Values.NonBoolean _) ->
                 Print.print ["bool",4] (fun p ->
                     p "bool ignores nonBoolean assignment");
                 (* Assignment is non-Boolean.
                        We don't care about it and see if we have something to say *)
                 speak machine { state with kernel }

              | SAssign((t,Top.Values.Boolean b) as bassign) ->
                 (* We extend our Boolean model with the assignment *)
                 match K.Model.add a state.fixed with
                 | Case2 msg ->
                    (* Model is inconsistent; kernel gives us unsat message *)
                    Print.print ["bool",4] (fun p ->
                        p "Model says this is inconsistent!");
                    Msg msg, machine { state with silent = true }

                 | Case1 fixed ->
                    (* Model is consistent. We update the undetermined variables. *)
                    let undetermined =
                      (* If the term was undetermined, we now have a value *)
                      if Assign.mem a state.undetermined
                      then (Print.print ["bool",4] (fun p ->
                                p "bool: was wondering about %a" SAssign.pp a);
                            Assign.remove a state.undetermined)
                      else state.undetermined
                    in
                    let undetermined = (* Same with negation *)
                      let a = SAssign.boolassign ~b:(not b) t in
                      if Assign.mem a undetermined
                      then
                        (Print.print ["bool",4] (fun p ->
                             p "bool: was also wondering about %a" SAssign.pp a);
                         Assign.remove a undetermined)
                      else undetermined
                    in
                    (* We declare to the watched literals 
                       that clauses watching l must be checked
                       - remember that clauses are watching 
                       the negations of the literals they contain *)
                    let watched = WL.fix bassign state.watched in
                    (* Let's look at what the kernel said. *)
                    match recorded with
                    | Case1 propas ->
                       (* Assignment was of a conjunctive kind,
                           kernel has given us the propagation messages
                           saying that a bunch of conjuncts are implied *)
                       let propas = List.fold Pqueue.push propas state.propas in
                       speak machine
                         { state with kernel; fixed; watched; propas; undetermined }
                    | Case2 c ->
                       (* Asignment was of a disjunctive kind.
                           We simplify the constraint according to our current model,
                           and give it to the watched literals *)
                       let c = Config.Constraint.simplify c fixed in
                       let open K in
                       match infer c with

                       | Falsified msg ->
                          Print.print ["bool",4] (fun p ->
                              p "bool: new constraint - kernel says %a" pp_message msg);
                          Msg msg, machine { state with silent = true }

                       | Unit msg ->
                          Print.print ["bool",4] (fun p ->
                              p "bool: new constraint - kernel says %a" pp_message msg);
                          let propas = Pqueue.push msg state.propas in
                          speak machine { state with kernel; fixed; watched;
                                                     undetermined; propas }
                       | Satisfied ->
                          Print.print ["bool",4] (fun p ->
                              p "bool: new constraint - kernel says %a is satisfied"
                                Constraint.pp c);
                          speak machine { state with kernel; fixed; watched;
                                                     undetermined }

                       | ToWatch(_,watchable) ->

                          Print.print ["bool",4] (fun p ->
                             p "bool: new constraint - Watching %a in clause %a"
                               (List.pp BAssign.pp) watchable
                               BAssign.pp bassign);
                          let watched = WL.addconstraint ~watched:watchable c watched in
                          (* By the way, the constraint's literals that are undetermined
                           need to be recorded, so we can pick one when we do a split *)
                          let newlits =
                            match Config.Constraint.simpl c with
                            | Some(newlits,_) -> newlits
                            | _ -> VarMap.empty
                          in
                          (* Getting a constraint's literals (and negations)
                             into an Assign.t*)
                          let aux var _ sofar =
                            if K.BMap.mem var (K.Model.map fixed)
                            then sofar
                            else
                              let sassign  = SAssign.boolassign ~b:true var in
                              let sassign' = SAssign.boolassign ~b:false var in
                              Assign.add sassign (Assign.add sassign' sofar)
                          in
                          let newlits = VarMap.fold aux newlits Assign.empty in
                          let undetermined = Assign.union newlits undetermined in
                          (* Constraint's literals that we have never seen before
                                 are given score !bump_value. *)
                          scores := BaMap.union_poly
                                      (fun _ () score -> score)
                                      (BaMap.map (fun _ () -> !bump_value))
                                      (fun scores -> scores)
                                      newlits
                                      !scores;
                          Print.print ["bool",5] (fun p ->
                              p "Cardinal of !scores: %i" (BaMap.cardinal !scores));
                          Print.print ["bool",6] (fun p ->
                              p "All scored lits\n%a" BaMap.pp !scores);
                          Print.print ["bool",5] (fun p ->
                              p "Just put scores for %a" Assign.pp newlits);
                          (* The undetermined ones are recorded as undetermined *)
                          (* And now we look at what we have to say *)
                          speak machine { state with kernel; fixed; watched;
                                                     undetermined });
      in

      let share =
        (* If we have already sent an unsat message, we shut up. *)
        if state.silent then fun _ -> Silence, machine state
        else
          fun tset ->
          let kernel = K.share tset state.kernel in
          speak machine { state with kernel }
      in
                                  
      let clone () = machine state in

      let watched _ = machine state in

      let suicide baset =
        scores := BaMap.union_poly (fun _ () v -> v +. !bump_value)
                    (fun _ -> BaMap.empty)
                    (fun scores -> scores)
                    baset !scores;
        Print.print ["bool",2] (fun p ->
            p "Cardinal of !scores: %i" (BaMap.cardinal !scores));
        incr since_last;
        bump_value := !bump_value *. decay;
        if !since_last > 1000
        then (scores := BaMap.map (fun _ score -> score /. factor) !scores;
              bump_value := !bump_value /. factor;
              since_last := 1)
      in

      let propose ?term _ =
        let remaining =
          BaMap.inter_poly (fun _ v () -> v) !scores state.undetermined
        in
        Print.print ["bool",6] (fun p ->
            p "bool: All scored assignments\n %a" BaMap.pp !scores);
        Print.print ["bool",5] (fun p ->
            p "bool: Choosing among %a" BaMap.pp remaining);
        match BaMap.info remaining with
        | Some(sassign,_),card ->
          Print.print ["bool",2] (fun p ->
              p "bool: kernel is not fine yet, proposing %a"
                SAssign.pp sassign);
          incr PFlags.decnumbB;
          PFlags.decwidth := !PFlags.decwidth + card;
          [sassign,1.0]
        | None,_ ->
          Print.print ["bool",0] (fun p ->
              p "bool: waiting for master to catch up");
          []
      in
      SlotMachine { add; share; clone; suicide; watched; propose }

                  
    let init = machine { kernel = K.init;
                         fixed  = K.Model.empty;
                         watched= WL.init;
                         propas = Pqueue.empty();
                         undetermined = Assign.empty;
                         silent = false }

    let clear () =
      Print.print ["bool",3] (fun p -> p "bool: clearing (including scores)");
      scores := BaMap.empty

  end

  let make (module K : API.API with type sign = sign)
    = let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end
