open General
open Sums
open Patricia
open Patricia_tools
open Patricia_interfaces
       
open Kernel
open Top.Specs
open Top.Messages
open Termstructures.Literals
open Termstructures.Clauses
open Theories.Bool

open Tools.PluginsTh
       
type sign = MyTheory.sign

              
module Make(WB:Export.WhiteBoard) = struct

  open WB
  open DS
  module Make(K: API.API with type sign = MyTheory.sign
                          and type assign  = Assign.t
                          and type bassign = Term.t * bool
                          and type sassign = Term.t * Value.t Top.Values.t)
    = struct

    type datatypes = Term.datatype*Value.t*Assign.t

    (* We are implementing VSIDS heuristics for choosing decisions:
       we need to keep a score of each bassign, we need to update the scores,
       and we need to pick the lit with highest score.
       We use a patricia tries for this. *)
                                             
    module ArgMap = struct
      include SAssign
      type values    = float
      type infos     = (SAssign.t*float) option
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
      let treeHCons = None
    end

    module ArgSet = struct
      include SAssign
      include EmptyInfo
      let treeHCons = None
    end

    module BaMap = struct
      include PatMap.Make(ArgMap)(TypesFromHConsed(ArgMap))
      let pp = print_in_fmt (fun fmt (l,_) -> pp_sassign fmt (SAssign.reveal l))
    end
                     
    module BaSet = struct
      include PatSet.Make(ArgSet)(TypesFromHConsed(ArgSet))
      let pp = print_in_fmt (fun fmt l -> pp_sassign fmt (SAssign.reveal l))
    end
                     
    let decay = 3.
    let factor = decay**1000.

    let scores = ref BaMap.empty
    let bump_value = ref 1.
    let since_last = ref 1

    let bump baset =
      scores := BaMap.union_poly (fun _ () v -> v +. !bump_value)
                  (* (LMap.map (fun _ () -> !bump_value)) *)
                  (fun _ -> BaMap.empty)
                  (fun scores -> scores)
                  baset !scores;
      Dump.print ["bool_pl1",2] (fun p ->
          p "Cardinal of !scores: %i" (BaMap.cardinal !scores));
      incr since_last;
      bump_value := !bump_value *. decay;
      if !since_last > 1000
      then (scores := BaMap.map (fun _ score -> score /. factor) !scores;
            bump_value := !bump_value /. factor;
            since_last := 1)

                                             
    type fixed = K.Model.t
                       
    (* Configuration for the 2-watched literals module *)
    module Config = struct
      module Constraint = K.Constraint
      module Var = LitF
      type nonrec fixed = fixed
      let simplify fixed = Constraint.simplify fixed
      let pick_another _ c i _ =
        match Constraint.simpl c with
        | Some(_,watchable) -> watchable
        | None -> []
    end

    (* 2-watched literals module *)
    module WL = TwoWatchedLits.Make(Config)

    type state = {
        kernel : K.state;      (* The state of the kernel *)
        fixed  : Config.fixed; (* Our Boolean model *)
        watched: WL.t;         (* The state of our watched literals *)
        propas : (K.sign,straight) Msg.t Pqueue.t; (* The propa messages we need to send*)
        undetermined : BaSet.t; (* The set of assignments we could fix *)
        (* Constraints that will be satisfied by propagations sent to master *)
        willbefine : K.Constraint.t Pqueue.t;
        silent : bool          (* Whether we have already sent an unsat message *)
      }

    (* We are asked whther we have something to say *)
    let rec speak machine state =
      (* First, we look at whether there are some propagation messages to send *)
      match Pqueue.pop state.propas with
      | Some(msg,q) ->
         (* ...in which case we send a propagation message *)
         Msg msg, machine { state with propas = q }
      | None -> 
         (* With no propagation message we ask the watched literals
            if a clause is weird *)
         let output,watched = WL.next state.fixed ~howmany:2 state.watched in
         match output with
         | None ->
            (* All clauses seem fine. Maybe the problem is sat ? *)
            Print.print ["bool",4] (fun p -> p "bool: Watched literals are done");
            (* We first force the kernel to forget the constraints that are already true *)
            let rec aux accu kernel willbefine = 
              match Pqueue.pop willbefine with
              | None -> accu, kernel
              | Some(c,willbefine) ->
                 let c = K.Constraint.simplify state.fixed c in
                 match K.infer c with
                 | K.Satisfied f -> aux accu (f kernel) willbefine
                 | _ -> aux (Pqueue.push c accu) kernel willbefine
            in
            let willbefine, kernel = aux (Pqueue.empty()) state.kernel state.willbefine in
            let state = { state with watched = watched;
                                     willbefine = willbefine;
                                     kernel = kernel }
            in
            (* Now we ask the kernel whether it still has any constraints to satisfy *)
            begin match K.sat kernel with
            | Some msg ->
               (* Kernel says all clauses are satisfied and gave us the message to send *)
               Msg msg, machine state

            | None ->
               (* Some clauses still need to be satisfied somehow. It's time to split. *)
               let remaining =
                 BaMap.inter_poly (fun _ v () -> v) !scores state.undetermined
               in
               Dump.print ["bool_pl1",3] (fun p ->
                   p "All scored lits\n%a" BaMap.pp !scores);
               Dump.print ["bool_pl1",2] (fun p ->
                   p "Choosing among %a" BaMap.pp remaining);
               let sassign =
                 SAssign.reveal(match BaMap.info remaining with
                                | Some(sassignh,_) -> sassignh
                                | None -> BaSet.choose state.undetermined)
               in
               Print.print ["bool",4] (fun p ->
                   p "bool: kernel is not fine yet, proposing %a" pp_sassign sassign);
               Try sassign, machine state
            end

         | Some(c,_) ->
            (* Watched literals have found a weird clause.
               Let's see what the kernel can make of it. *)
            let open K in
            match infer c with
            | Falsified msg ->
               (* Clause is false and kernel gave us the unsat message to send. *)
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" Msg.pp msg);
               Msg msg, machine { state with watched = watched;
                                             silent = true }
            | Unit msg ->
               (* Clause is unit and kernel gave us the propagation message to send. *)
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" Msg.pp msg);
               let willbefine = Pqueue.push c state.willbefine in
               Msg msg, machine { state with watched = watched;
                                             willbefine = willbefine }
            | Satisfied f ->
               (* Clause is satisfied. We tell the kernel to update its state
                  and look for something else to say*)
               Print.print ["bool",4] (fun p ->
                   p "bool: kernel says %a is satisfied" Constraint.pp c);
               speak machine { state with watched = watched;
                                          kernel = f state.kernel }

    
    (* This is the main loop. *)                   
    let rec machine state =
      SlotMachine {
          add =
            ((* If we have already sent an unsat message, we shut up. *)
             if state.silent then fun _ -> Silence, machine state
             else function
               | None -> (* We haven't been given any new information.
                            Let's see if we have something to say *)
                  Print.print ["bool",2] (fun p -> p "bool receiving useless stuff");
                  speak machine state

               | Some a ->
                  Print.print ["bool",2] (fun p ->
                      p "bool receiving Some(%a)" pp_sassign a);
                  (* We ask the kernel to record the new assignment *)
                  let recorded,kernel = K.add a state.kernel in
                  let t,v = a in
                  match v with
                  | Top.Values.NonBoolean _ ->
                     Print.print ["bool",4] (fun p ->
                         p "bool ignores nonBoolean assignment");
                     (* Assignment is non-Boolean.
                       We don't care about it and see if we have something to say *)
                     speak machine { state with kernel = kernel }
                  | Top.Values.Boolean b ->
                     let undetermined =
                       (* If the term was undetermined, we now have a value *)
                       let sassignh = SAssign.build a in
                       if BaSet.mem sassignh state.undetermined
                       then
                         (Print.print ["bool",4] (fun p ->
                              p "bool: was wondering about %a" Term.pp t);
                          let sassign'  = Top.Values.bassign ~b:(not b) t in
                          let sassignh' = SAssign.build sassign' in
                          BaSet.remove sassignh
                            (BaSet.remove sassignh' state.undetermined))
                       else state.undetermined
                     in
                     let bassign = t,b in
                     (* We extend our Boolean model with the assignment *)
                     match K.Model.add bassign state.fixed with
                     | Case2 msg ->
                        (* Model is inconsistent; kernel gives us unsat message *)
                        Print.print ["bool",4] (fun p ->
                            p "Model says this is inconsistent!");
                        Msg msg, machine { state with silent = true }

                     | Case1(l,fixed) ->
                        (* Model is consistent. We declare to the watched literals
                        that 2 literals have been fixed *)
                        let watched = WL.fix l (WL.fix (LitF.negation l) state.watched) in
                        (* Let's look at what the kernel said. *)
                        match recorded with
                        | Some propas ->
                           (* Assignment was of a conjunctive kind,
                           kernel has given us the propagation messages
                           saying that a bunch of conjuncts are implied *)
                           let propas = List.fold Pqueue.push propas state.propas in
                           speak machine { state with kernel  = kernel;
                                                      watched = watched;
                                                      fixed   = fixed;
                                                      propas  = propas }
                        | None ->
                           (* Asignment was of a disjunctive kind.
                           We create the constraint we need to satisfy,
                           simplify it according to our current model,
                           and give it to the watched literals *)
                           let constr = Config.Constraint.make bassign in
                           let constr = Config.Constraint.simplify fixed constr in
                           let watched = WL.addconstraintNflag constr watched in
                           (* By the way, the constraint's literals that are undetermined
                           need to be recorded, so we can pick one when we do a split *)
                           let newlits =
                             match Config.Constraint.simpl constr with
                             | Some(newlits,_) -> newlits
                             | _ -> LSet.empty
                           in
                           let aux lit sofar =
                             let _,i = LitF.reveal lit in
                             let t = Term.term_of_id i in
                             let sassignh = SAssign.build(Top.Values.bassign t) in
                             let sassignh' = SAssign.build(Top.Values.bassign ~b:(not b) t) in
                             BaSet.add sassignh (BaSet.add sassignh' sofar)
                           in
                           let newsassign = LSet.fold aux newlits BaSet.empty in
                           let undetermined = BaSet.union newsassign undetermined in

                           Dump.print ["bool",2] (fun p ->
                               p "Putting scores for %a"
                                 (LSet.print_in_fmt LitF.print_in_fmt) newlits);
                           (* Every literal in newlits that we have never seen 
                           before is given score !bump_value. *)
                           scores := BaMap.union_poly
                                       (fun _ () score -> score)
                                       (BaMap.map (fun _ () -> !bump_value))
                                       (fun scores -> scores)
                                       newsassign
                                       !scores;
                           Dump.print ["bool",2] (fun p ->
                               p "Cardinal of !scores: %i" (BaMap.cardinal !scores));
                           Dump.print ["bool",3] (fun p ->
                               p "All scored lits\n%a" BaMap.pp !scores);
                           
                           (* And now we look at what we have to say *)
                           speak machine { state with kernel = kernel;
                                                      fixed  = fixed;
                                                      watched= watched;
                                                      undetermined = undetermined } );
          
          clone   = (fun () -> machine state);
          suicide = (fun _ -> ())
        }
                  
    let init = machine { kernel = K.init;
                         fixed  = K.Model.empty;
                         watched= WL.init;
                         propas = Pqueue.empty();
                         undetermined = BaSet.empty;
                         willbefine = Pqueue.empty();
                         silent = false }

    let clear () = K.clear ()

  end

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end
