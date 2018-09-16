open General
open Patricia
open Patricia_tools

open Kernel
open Top.Terms
open Top.Sassigns
open Top.Messages
open Termstructures.Rationals
open Theories.Theory
open Theories.LRA

open Tools
       
type sign = MyTheory.sign
type api  = (module API.API with type sign = MyTheory.sign)

let hdl = MyTheory.hdl

module Make(W: Writable) = struct

  module Make(K: API.API with type sign = MyTheory.sign) = struct

    module DT = Datatypes.Make(W)(K)
    open DT

    type state = {
        kernel : K.state;  (* The state of the kernel *)
        fixed  : K.Model.t;(* Our LRA valuation *)
        watchedB: WLB.t;   (* Watched literals for constraints *)
        watchedQ: WLQ.t;   (* Watched literals for terms to evaluate *)
        domains: Domain.t; (* The set of variables we could fix, with their domains *)
        propas : (K.sign,straight) message Pqueue.t; (* The propa messages to be send*)
        umsg   : (K.sign,unsat) message option;      (* The unsat message to be send*)
        silent : bool      (* Whether we have already sent an unsat message *)
      }


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
         | Case1 _ ->

            let output,watchedQ = WLQ.next state.fixed ~howmany:1 state.watchedQ in
            begin match output with
            | Case1 _ ->
               Print.print ["LRA",4] (fun p -> p "LRA: Watched literals are done");
               (* We ask the kernel whether it still has any constraints to satisfy *)
               let kernel, msg = K.sat state.fixed state.kernel in
               let state = { state with watchedB; watchedQ; kernel } in
               begin match msg with
               | Some msg ->
                 (* Kernel says all is satisfied and gave us the message to send *)
                 Msg msg, machine state

               | None ->
                 (* Some constraints still need to be satisfied somehow. *)
                 Silence, machine state
               end

            | Case2((c,q,_),_) ->
               (* Watched literals have found a term whose value is entirely determined.
               Let's see what the kernel can make of it. *)
               Print.print ["LRA",4] (fun p ->
                   p "LRA: Watched lits have found determined term %a" K.Simpl.pp c);
               let state = { state with watchedB; watchedQ } in
               let open K in
               match eval c with

               | Beval msg ->
                  (* c of sort Bool does evaluate - we send the propagation message *)
                  Print.print ["LRA",4] (fun p -> p "LRA: kernel says %a" pp_beval msg);
                  Msg msg, machine state

               | Qeval(term,q') ->
                  begin match q with
                  | None -> speak machine state
                     (* let v = Top.Values.NonBoolean(K.vinj q') in
                      * let sassign = SAssign(term,v) in
                      * let i = Term.id term in
                      * if K.VarMap.mem i (K.Model.map state.fixed)
                      * then speak machine state
                      * else
                      *   (Print.print ["LRA",2] (fun p ->
                      *        p "LRA: new value! %a" pp_sassign sassign);
                      *    Try sassign, machine state ) *)
                  | Some q when Q.equal q q' -> speak machine state
                  | Some q                   -> failwith "SMA problem!"
                  end
                    
               | Unit _ | ToWatch _ -> failwith "Watched Literals got it wrong"
            end

         | Case2((c,b),_) ->
            (* Watched literals have found a weird constraint.
            Let's see what the kernel can make of it. *)
            Print.print ["LRA",4] (fun p ->
                p "LRA: Watched lits have found weird constraint %s(%a)"
                  (if b then "" else "~") K.Simpl.pp c);
            let open K in
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
               let bassign  = Simpl.term c, Top.Values.Boolean b in
               let oldrange = Domain.find var state.domains in
               Print.print ["LRA",4] (fun p ->
                   p "LRA: old range for %a is %a"
                     Term.pp var Range.pp oldrange);
               let range =
                 let is_strict = [%eq: bool] b in
                 let update original =
                   let update = if [%eq: bool] is_coeff_pos b
                                then Range.upper_update
                                else Range.lower_update
                   in update bound ~is_strict:(is_strict original) bassign oldrange
                 in
                 match nature,b with
                 | Lt,_ -> update true
                 | Le,_ -> update false
                 | NEq,true | Eq,false -> Range.diseq_update bound bassign oldrange
                 | Term,_ | Other,_ -> raise IdontUnderstand
                 | Eq,true | NEq,false ->
                    match Range.lower_update bound ~is_strict:false bassign oldrange with
                    | Range.Range range ->
                       Range.upper_update bound ~is_strict:false bassign range
                    | ans -> ans
               in
               match range with
               | Range.Range range ->
                  Print.print ["LRA",4] (fun p ->
                      p "LRA: new range for %a is %a" Term.pp var Range.pp range);
                  let domains = Domain.add var (fun _ -> range) state.domains in
                  speak machine { state with watchedB; domains }

               | Range.FourierMotzkin(ba1,ba2) ->
                  let (Propa(_,Straight(t,_))) as msg = fm ba1 ba2 var in
                  let c = Simpl.simplify state.fixed (Simpl.make t) in
                  Print.print ["LRA",0] (fun p -> p "%a" pp_fm (ba1,ba2,t));
                  (match eval c with
                   | Beval msg_semantic ->
                      let propas = state.propas
                                   |> Pqueue.push msg_semantic
                                   |> Pqueue.push msg
                      in speak machine { state with watchedB; propas }
                   | _ -> failwith "Fourier-Motzkin got it wrong")
                  
               | Range.DisEqual(ba1,ba2,ba3) ->
                  let a1, a2, msg = disequal ba1 ba2 ba3 var in
                  let a1' = Simpl.simplify state.fixed (Simpl.make a1) in
                  let a2' = Simpl.simplify state.fixed (Simpl.make a2) in
                  match eval a1', eval a2' with
                  | Beval msg1, Beval msg2 ->
                     Print.print ["LRA",0] (fun p ->
                         p "%a" pp_diseq (ba1,ba2,ba3,msg1,msg2));
                     let propas = state.propas
                                  |> Pqueue.push msg1
                                  |> Pqueue.push msg2
                     in speak machine { state with watchedB; propas; umsg = Some msg }
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
                 p "LRA receiving Some(%a)" SAssign.pp sassign);
             (* We ask the kernel to record the new assignment *)
             let kernel, recorded = K.add sassign state.kernel in
             match recorded with
             | None ->
                Print.print ["LRA",2] (fun p -> p "LRA receiving useless stuff");
                speak machine { state with kernel }
             | Some(SAssign(c,v)) ->
                let fixed = K.Model.add sassign state.fixed in
                let i = K.Simpl.term c in
                if VarMap.mem i (K.Simpl.coeffs c)
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
                    let newvariables =
                      VarMap.diff_poly (fun _ _ _ -> VarMap.empty)
                        (K.Simpl.coeffs c) (K.Model.map fixed)
                    in
                    let domains = Domain.union_poly
                                    (fun _ old _ -> old)
                                    (fun old -> old)
                                    (Domain.map (fun _ _ -> Range.init))
                                    state.domains
                                    newvariables
                    in
                    match v with
                    | Top.Values.NonBoolean q ->
                       let watchedQ =
                         WLQ.addconstraintNflag (c,Some q,i) state.watchedQ
                       in
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
            WLQ.addconstraintNflag (c,None,K.Simpl.term c) watchedQ,
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

      let suicide _ = () in

      let propose ?term _ =
        match Domain.info state.domains with
        | None ->
          Print.print ["LRA",2] (fun p ->
              p "LRA: waiting for master to catch up");
          []

        | Some var ->
          let range = Domain.find var state.domains in
          let v = Top.Values.NonBoolean(K.vinj(Range.pick range)) in
          let sassign = SAssign.build(var,v) in
          Print.print ["LRA",2] (fun p ->
              p "LRA: kernel is not fine yet, proposing %a from range %a"
                SAssign.pp sassign Range.pp range);
          [sassign,1.0]
      in

      SlotMachine { add; share; clone; suicide; propose }

    let init = machine { kernel = K.init;
                         fixed  = K.Model.empty;
                         watchedB= WLB.init;
                         watchedQ= WLQ.init;
                         domains = Domain.empty;
                         propas = Pqueue.empty();
                         umsg   = None;
                         silent = false }

    let clear () = Print.print ["LRA",3] (fun p -> p "LRA: clearing")

  end
        
  let make (module K : API.API with type sign = MyTheory.sign)
    = let module Made = Make(K) in
      { PluginTh.init = Made.init;
        PluginTh.clear = Made.clear }

end
