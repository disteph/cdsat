open General
open Patricia
open Patricia_tools

open Kernel
open Export
open Termstructures.Rationals
open Top.Specs
open Top.Basic
open Top.Sassigns
open Top.Messages
open Theories.LRA

open Tools.PluginsTh
       
type sign = MyTheory.sign

module Make(DS: GlobalImplem) = struct
  open DS
  module Make(K: API.API with type sign   = MyTheory.sign
                          and type assign = Assign.t
                          and type termdata= Term.datatype
                          and type value  = Value.t
                          and type tset   = TSet.t )
    = struct

    type datatypes = Term.datatype*Value.t*Assign.t*TSet.t

    module Arg = struct
      type t = int [@@deriving ord]
      let id i = i
      type values = bassign Range.t
      include MaxInfo(struct type t = int [@@deriving ord] end)
      let treeHCons = None
    end

    module Domain = PatMap.Make(Arg)(TypesFromHConsed(Arg))
                                                      
    type fixed = K.Model.t

    module ConfigB = struct
      module Constraint = struct
        type t = K.Simpl.t * bool [@@deriving show]
        let id (c,b) =
          let t = K.Simpl.term c in
          2*(Term.id t)+(if b then 1 else 0)
      end
      module Var = struct
        type t = int [@@deriving ord]
        let pp fmt i = Term.pp fmt (Term.term_of_id i)
        let show = Print.stringOf pp
      end
      type nonrec fixed = fixed
      let simplify fixed (c,b) = K.Simpl.simplify fixed c, b
      let pick_another _ (c,_) i _ =
        Print.print ["LRA",2] (fun p ->
            p "LRA: WLB picks variables for %a, gets %a"
              K.Simpl.pp c
              (List.pp Term.pp)
              (List.map Term.term_of_id (K.Simpl.watchable c)));
        K.Simpl.watchable c
    end

    module WLB = TwoWatchedLits.Make(ConfigB)

    module ConfigQ = struct
      module Constraint = struct
        type t = K.Simpl.t * (Top.Qhashed.t option) * int [@@deriving show]
        let id (_,_,i) = i
      end
      module Var = struct
        type t = int [@@deriving ord]
        let pp fmt i = Term.pp fmt (Term.term_of_id i)
        let show = Print.stringOf pp
      end
      type nonrec fixed = fixed
      let simplify fixed (c,v,i) = K.Simpl.simplify fixed c,v,i
      let pick_another _ (c,_,_) i _ =
        Print.print ["LRA",2] (fun p ->
            p "LRA: WLQ picks variables for %a, gets %a"
              K.Simpl.pp c
              (List.pp Term.pp)
              (List.map Term.term_of_id (K.Simpl.watchable c)));
        K.Simpl.watchable c
    end

    module WLQ = TwoWatchedLits.Make(ConfigQ)

    type state = {
        kernel : K.state;  (* The state of the kernel *)
        fixed  : fixed;    (* Our LRA valuation *)
        watchedB: WLB.t;   (* Watched literals for constraints *)
        watchedQ: WLQ.t;   (* Watched literals for terms to evaluate *)
        domains: Domain.t; (* The set of variables we could fix, with their domains *)
        propas : (K.sign,straight) Msg.t Pqueue.t; (* The propa messages to be send*)
        umsg   : (K.sign,unsat) Msg.t option;      (* The unsat message to be send*)
        silent : bool      (* Whether we have already sent an unsat message *)
      }

    (* pretty printing an evaluation inference *)
    let pp_beval fmt msg =
      let Propa(justif,Straight(t,Top.Values.Boolean b)) = msg in
      Format.fprintf fmt "%a ⊢ %a"
        Assign.pp justif
        (Top.Sassigns.pp_sassign K.Simpl.pp Q.pp_print)
        (SAssign(K.Simpl.make t,Top.Values.Boolean b))
                   

    (* We are asked whether we have something to say *)
    let rec speak machine state =
      Print.print ["LRA",5] (fun p ->
          p "LRA: We are asked whether we have something to say");

      match Pqueue.pop state.propas, state.umsg with
      | _ when state.silent -> Silence, machine state
      | Some(msg,propas), _ -> Msg msg, machine { state with propas }
      | None, Some msg      -> Msg msg, machine { state with silent = true }
      | None, None ->

         let output,watchedB = WLB.next state.fixed ~howmany:2 state.watchedB in
         match output with
         | None ->

            let output,watchedQ = WLQ.next state.fixed ~howmany:1 state.watchedQ in
            begin match output with
            | None ->
               Print.print ["LRA",4] (fun p -> p "LRA: Watched literals are done");
               (* We ask the kernel whether it still has any constraints to satisfy *)
               let kernel, msg = K.sat state.fixed state.kernel in
               let state = { state with watchedB; watchedQ; kernel } in
               begin match msg with
               | Some msg ->
                  (* Kernel says all is satisfied and gave us the message to send *)
                  Msg msg, machine { state with watchedB; watchedQ; kernel }

               | None ->
                  (* Some constraints still need to be satisfied somehow.
                     Time to decide. *)
                  match Domain.info state.domains with
                  | None ->
                     Print.print ["LRA",0] (fun p ->
                         p "LRA: waiting for master to catch up");
                     Silence, machine state

                  | Some var ->
                     let range = Domain.find var state.domains in
                     let v = Top.Values.NonBoolean(K.vinj(Range.pick range)) in
                     let sassign = SAssign(Term.term_of_id var,v) in
                     Print.print ["LRA",2] (fun p ->
                         p "LRA: kernel is not fine yet, proposing %a from range %a"
                           pp_sassign sassign Range.pp range);
                     Try sassign, machine state
               end

            | Some((c,q,_),_) ->
               (* Watched literals have found a term whose value is entirely determined.
               Let's see what the kernel can make of it. *)
               let open K in
               Print.print ["LRA",4] (fun p ->
                   p "LRA: Watched lits have found determined term %a"
                     K.Simpl.pp c);
               match eval c with

               | Beval msg ->
                  (* c of sort Bool does evaluate - we send the propagation message *)
                  Print.print ["LRA",4] (fun p -> p "LRA: kernel says %a" pp_beval msg);
                  Msg msg, machine { state with watchedB; watchedQ }

               | Qeval(term,q') ->
                  begin match q with
                  | None ->
                     let v = Top.Values.NonBoolean(K.vinj q') in
                     let sassign = SAssign(term,v) in
                     let i = Term.id term in
                     if K.VarMap.mem i (K.Model.map state.fixed)
                     then speak machine { state with watchedB; watchedQ }
                     else
                       (Print.print ["LRA",2] (fun p ->
                            p "LRA: new value! %a" pp_sassign sassign);
                        Try sassign, machine { state with watchedB; watchedQ })
                  | Some q when Q.equal q q' ->
                     speak machine { state with watchedB; watchedQ }
                  | Some q -> failwith "TODO"
                  end
                    
               | Unit _ | ToWatch _ -> failwith "Watched Literals got it wrong"
            end

         | Some((c,b),_) ->
            (* Watched literals have found a weird constraint.
            Let's see what the kernel can make of it. *)
            let open K in
            Print.print ["LRA",4] (fun p ->
                p "LRA: Watched lits have found weird constraint %s(%a)"
                  (if b then "" else "~")
                  K.Simpl.pp c);
            match eval c with
            | Beval msg ->
               let Propa(_,Straight(_,Top.Values.Boolean b')) = msg in
               Print.print ["LRA",4] (fun p -> p "LRA: kernel says %a" pp_beval msg);
               if [%eq : bool] b b'
               then speak machine { state with watchedB }
               else Msg msg, machine { state with watchedB }
            | Qeval _   -> failwith "Should be of sort Prop"
            | ToWatch _ -> failwith "Watched Literals got it wrong"
            | Unit{ var; nature; is_coeff_pos; bound } ->
               let bassign  = K.Simpl.term c, Top.Values.Boolean b in
               let oldrange = Domain.find var state.domains in
               Print.print ["LRA",4] (fun p ->
                   p "LRA: old range for %a is %a"
                     Term.pp (Term.term_of_id var) Range.pp oldrange);
               let range =
                 let is_strict = [%eq: bool] b in
                 let update original =
                   let update = if [%eq: bool] is_coeff_pos b
                                then Range.upper_update
                                else Range.lower_update
                   in update bound ~is_strict:(is_strict original) bassign oldrange
                 in
                 let open TS in
                 match nature,b with
                 | Lt,_ -> update true
                 | Le,_ -> update false
                 | NEq,true | Eq,false -> Range.diseq_update bound bassign oldrange
                 | Term,_ | Other,_ -> raise K.IdontUnderstand
                 | Eq,true | NEq,false ->
                    match Range.lower_update bound ~is_strict:false bassign oldrange with
                    | Range.Range range ->
                       Range.upper_update bound ~is_strict:false bassign range
                    | ans -> ans
               in
               match range with
               | Range.Range range ->
                  Print.print ["LRA",4] (fun p ->
                      p "LRA: new range for %a is %a"
                        Term.pp (Term.term_of_id var) Range.pp range);
                  let domains = Domain.add var (fun _ -> range) state.domains in
                  speak machine { state with watchedB; domains }
               | Range.FourierMotzkin(ba1,ba2) ->
                  let msg = fm ba1 ba2 var in
                  Print.print ["LRA",4] (fun p ->
                      let t1, Top.Values.Boolean b1 = ba1 in
                      let t2, Top.Values.Boolean b2 = ba2 in
                      let Propa(_,Straight(t,_)) = msg in
                      p "LRA: Found Fourier-Motzkin inference to make: %a, %a ⊢ %a"
                        (Top.Sassigns.pp_sassign Simpl.pp Q.pp_print)
                        (SAssign(Simpl.make t1,Top.Values.Boolean b1))
                        (Top.Sassigns.pp_sassign Simpl.pp Q.pp_print)
                        (SAssign(Simpl.make t2,Top.Values.Boolean b2))
                        Simpl.pp(Simpl.make t));
                  Msg msg, machine { state with watchedB }
               | Range.DisEqual(ba1,ba2,ba3) ->
                  let a1, a2, msg = disequal ba1 ba2 ba3 var in
                  let a1' = Simpl.simplify state.fixed (Simpl.make a1) in
                  let a2' = Simpl.simplify state.fixed (Simpl.make a2) in
                  match eval a1', eval a2' with
                  | Beval msg1, Beval msg2 ->
                     Print.print ["LRA",4] (fun p ->
                         let t1, Top.Values.Boolean b1 = ba1 in
                         let t2, Top.Values.Boolean b2 = ba2 in
                         let t3, Top.Values.Boolean b3 = ba3 in
                         p "LRA: Found Disequal inference to make:\n %a\n %a\n %a, %a, %a, %a, %a ⊢ ⊥"
                           pp_beval msg1
                           pp_beval msg2
                           (Top.Sassigns.pp_sassign Simpl.pp Q.pp_print)
                           (SAssign(Simpl.make t1,Top.Values.Boolean b1))
                           (Top.Sassigns.pp_sassign Simpl.pp Q.pp_print)
                           (SAssign(Simpl.make t2,Top.Values.Boolean b2))
                           (Top.Sassigns.pp_sassign Simpl.pp Q.pp_print)
                           (SAssign(Simpl.make t3,Top.Values.Boolean b3))
                           Simpl.pp(Simpl.make a1)
                           Simpl.pp(Simpl.make a2));
                     let propas = state.propas |> Pqueue.push msg1 |> Pqueue.push msg2 in
                     speak machine { state with watchedB; propas; umsg = Some msg }
                  | _ -> failwith "Diseq got it wrong"
                                  
    (* This is the main loop. *)                   
    let rec machine state =

      let add =
        (* If we have already sent an unsat message, we shut up. *)
        if state.silent then fun _ -> Silence, machine state
        else function
          | None -> (* We haven't been given any new information.
                            Let's see if we have something to say *)
             Print.print ["LRA",2] (fun p -> p "LRA receiving nothing");
             speak machine state

          | Some sassign ->
             Print.print ["LRA",2] (fun p ->
                 p "LRA receiving Some(%a)" pp_sassign sassign);
             (* We ask the kernel to record the new assignment *)
             let kernel, recorded = K.add sassign state.kernel in
             match recorded with
             | None ->
                Print.print ["LRA",2] (fun p -> p "LRA receiving useless stuff");
                speak machine { state with kernel }
             | Some(SAssign(c,v)) ->
                let fixed = K.Model.add sassign state.fixed in
                let i = Term.id(K.Simpl.term c) in
                if TS.VarMap.mem i (K.Simpl.coeffs c)
                then
                  begin
                    Print.print ["LRA",2] (fun p ->
                        p "LRA fixes variable %a" Term.pp (K.Simpl.term c));
                    let domains = 
                      if Domain.mem i state.domains
                      then Domain.remove i state.domains
                      else state.domains
                    in
                    let watchedB = WLB.fix i state.watchedB in
                    let watchedQ = WLQ.fix i state.watchedQ in
                    speak machine
                      { state with kernel; fixed; watchedB; watchedQ; domains }
                  end
                else
                  begin
                    let c = K.Simpl.simplify state.fixed c in
                    Print.print ["LRA",2] (fun p ->
                        p "LRA watches %a" Term.pp (K.Simpl.term c));
                    let domains = Domain.union_poly
                                    (fun _ old _ -> old)
                                    (fun old -> old)
                                    (Domain.map (fun _ _ -> Range.init))
                                    state.domains
                                    (K.Simpl.coeffs c)
                    in
                    match v with
                    | Top.Values.NonBoolean q ->
                       let watchedQ = WLQ.addconstraintNflag (c,Some q,i) state.watchedQ in
                       speak machine { state with kernel; fixed; watchedQ; domains }

                    | Top.Values.Boolean b ->
                       let watchedB = WLB.addconstraintNflag (c,b) state.watchedB in
                       speak machine { state with kernel; fixed; watchedB; domains }
                  end
      in

      let share =
        (* If we have already sent an unsat message, we shut up. *)
        if state.silent then fun _ -> Silence, machine state
        else
          fun tset ->
          let kernel, new2evaluate = K.share tset state.kernel in
          let aux c (watchedQ,domains) =
            let c = K.Simpl.simplify state.fixed c in
            WLQ.addconstraintNflag (c,None,Term.id(K.Simpl.term c)) watchedQ,
            Domain.union_poly
              (fun _ old _ -> old)
              (fun old -> old)
              (Domain.map (fun _ _ -> Range.init))
              domains
              (K.Simpl.coeffs c)
          in
          let watchedQ,domains =
            List.fold aux new2evaluate (state.watchedQ,state.domains)
          in
          speak machine { state with kernel; watchedQ; domains }
      in
      
      let clone () = machine state in

      let suicide _ = ()

      in SlotMachine { add; share; clone; suicide }

    let init = machine { kernel = K.init;
                         fixed  = K.Model.empty;
                         watchedB= WLB.init;
                         watchedQ= WLQ.init;
                         domains = Domain.empty;
                         propas = Pqueue.empty();
                         umsg   = None;
                         silent = false }

    let clear () =
      Print.print ["LRA",3] (fun p -> p "LRA: clearing");
      K.clear ()

  end
        
  let make (k: (Term.datatype,Value.t,Assign.t,TSet.t) MyTheory.api)
    = let (module K) = k in
      let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end
