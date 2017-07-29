open General
open Sums
open Patricia
open Patricia_tools
       
open Kernel
open Top.Specs
open Top.Messages
open Termstructures.Literals
open Termstructures.Clauses
open Theories.Bool

open Tools.PluginsTh
       
type sign = MyTheory.sign

module Make(DS:GlobalDS) = struct

  open DS
  module Make(K: API.API with type sign = MyTheory.sign
                          and type assign  = Assign.t
                          and type bassign = Term.t * bool
                          and type sassign = Term.t * Value.t Top.Values.t)
    = struct

    type datatypes = Term.datatype*Value.t*Assign.t

    type fixed = K.Model.t
                       
    (* Configuration for the 2-watched literals module *)
    module Config = struct
      module Constraint = K.Constraint
      module Var = LitF
      type nonrec fixed = fixed
      let simplify fixed = Constraint.simplify fixed
      let pick_another _ c i _ =
        match Constraint.simpl c with
        | Some(_,watchable) when List.length watchable >= i -> Some watchable
        | _ -> None
    end

    (* 2-watched literals module *)
    module WL = TwoWatchedLits.Make(Config)

    (* Set of terms *)
    module Arg = struct
      include Term
      include EmptyInfo
      let treeHCons = None
    end

    module TSet = PatSet.Make(Arg)(TypesFromHConsed(Term))
                                   
    type state = {
        kernel : K.state;      (* The state of the kernel *)
        fixed  : Config.fixed; (* Our Boolean model *)
        watched: WL.t;         (* The state of our watched literals *)
        propas : (K.sign,straight) Msg.t Pqueue.t; (* The propa messages we need to send*)
        undetermined : TSet.t; (* The set of Boolean terms whose value would help *)
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
            begin match K.sat state.kernel with
            | Some msg ->
               (* Kernel says all clauses are satisfied and gave us the message to send *)
               Msg msg, machine { state with watched = watched }
            | None ->
               (* Some clauses still need to be satisfied somehow. It's time to split. *)
               let sassign = TSet.choose state.undetermined,
                             Top.Values.Boolean false
               in
               Try sassign, machine { state with watched = watched }
            end
         | Some(c,_) ->
            (* Watched literals have found a weird clause.
               Let's see what the kernel can make of it. *)
            let open K in
            match infer c with
            | Falsified msg ->
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" Msg.pp msg);
               (* Clause is false and kernel gave us the unsat message to send. *)
               Msg msg, machine { state with watched = watched;
                                             silent = true }
            | Unit msg ->
               Print.print ["bool",4] (fun p -> p "bool: kernel says %a" Msg.pp msg);
               (* Clause is unit and kernel gave us the propagation message to send. *)
               Msg msg, machine { state with watched = watched }
            | Satisfied f ->
               Print.print ["bool",4] (fun p ->
                   p "bool: kernel says %a is satisfied" Constraint.pp c);
               (* Clause is satisfied. We tell the kernel to update its state
                  and look for something else to say*)
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
                       if TSet.mem t state.undetermined
                       then
                         (Print.print ["bool",4] (fun p ->
                              p "bool: was wondering about %a" Term.pp t);
                          TSet.remove t state.undetermined)
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

                     | Case1(l,nl,fixed) ->
                        (* Model is consistent. We declare to the watched literals
                        that 2 literals have been fixed *)
                        let watched = WL.fix l (WL.fix nl state.watched) in
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
                           let aux l =
                             let _,i = LitF.reveal l in
                             TSet.add (Term.term_of_id i)
                           in
                           let newterms = LSet.fold aux newlits TSet.empty in
                           let undetermined = TSet.union undetermined newterms in
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
                         undetermined = TSet.empty;
                         silent = false }

    let clear () = K.clear ()

  end

  let make (k: (Term.datatype,Value.t,Assign.t) MyTheory.api)
    = let (module K) = k in
      let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end
