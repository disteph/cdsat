open General
open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Interfaces_plugin
open Patricia
open Patricia_interfaces
open SetConstructions
open Sums

open PluginsG_tools.Addressing

(* TODO: integrate watched lit for learnt clauses *)

module DS = DataStructures

let count = [|0;0;0;0|]

  (* Flag decide whether we do
     - binary decide rules, using cut (decide_cut is true)
     - or random focus on remaining clauses (decide_cut is false)
  *)
let decide_cut = true
  
module Strategy(FE:FrontEndType with type IForm.datatype = DS.UF.t
				and  type FSet.ps     = DS.UFSet.t
				and  type ASet.ps     = DS.UASet.t) = struct
  open DS
  open FE
  include PluginsG_tools.Utils.FEext(FE)
  module Me = PluginsG_tools.Memoisation.Make(FE)(UFSet)(UASet)

  (******************************************)
  (* Setting up Unit Propagation mechanisms *)

  module UPConfig = struct
    module Constraint = struct
      type t = IForm.t*(UASet.t*UASet.t) option
      let id (form,_) = IForm.id form
    end
    module Var = LitF
    type fixed = UASet.t

    let simplify fixed (f,clause) =
      f,
      match clause with
      | Some(set,setn) when UASet.is_empty (UASet.inter setn fixed) ->
         let just_fixed = UASet.negations(UASet.inter set fixed) in
         let updated_clause = UASet.diff set fixed in
         Some(updated_clause,UASet.diff setn just_fixed)
      | _ -> None
      
    let pick_another fclause var =
      match fclause with
      | _,Some(set,_) ->
         let tochoose = 
           if UASet.mem var set
           then UASet.remove var set
           else set
         in
         if UASet.is_empty tochoose
         then None
         else Some(UASet.choose tochoose)
      | _ -> None
  end

  module UP = PluginsTh_tools.TwoWatchedLits.Make(UPConfig)


  (*************)
  (* Addresses *)

  type my_data = {i: int;
                  restore_parity : bool;
                  (* Remaining literals: may be watched and aren't
                  fixed. remlits is closed under negation *)
                  remlits : UASet.t;
                  (* Known Forms: we have already checked if they are
                  clauses, in which case they are watching literals *)
                  knownforms : UFSet.t;
                  (* Stack of forms that haven't been checked *)
                  stack : IForm.t list;
                  (* State of the 2-watched lits algorithm *)
                  up_state : UP.t;
                  (* Indicates that the next step should start with 
                     a memoisation step *)
                  mem_first : bool;
                  (* Model and formulae before last decide *)
                  modelb4   : ASet.t;
                  formsb4   : FSet.t
                 }
  type data       = my_data addressing
  let initial_data _ = ad_init {i = 0;
                                restore_parity = false;
                                remlits      = UASet.empty;
                                knownforms   = UFSet.empty;
                                stack        = [];
                                up_state     = UP.init;
                                mem_first    = false;
                                modelb4      = ASet.empty;
                                formsb4      = FSet.empty
                               }
  let address     = ref No

  (*************)
  (* Reporting *)

  let report i =
    print_endline("   Plugin's report (DPLL):");
    print_endline(string_of_int count.(0)^" notifies, "^
		    string_of_int count.(1)^" Backtrack, "^
		    string_of_int count.(2)^" Unit propagate, "^
		    string_of_int count.(3)^" Decide, "^
		    string_of_int (Dump.Plugin.read_count 7)^" Memo backtrack, "^
		    string_of_int (Dump.Plugin.read_count 8)^" Memo UP "); 
    Me.report();
    print_endline ""

  (******************)
  (* Restarts stuff *)

  module Restarts = PluginsG_tools.RestartStrategies.RestartStrategies(UASet)
  let restart_strategy = Restarts.getbyname !Flags.restarts_strategy !Flags.restarts_p1 !Flags.restarts_p2    
  let last_model = ref UASet.empty

  (******************************************************)
  (* Last resort: pick a clause and put the focus on it *)
	
  let clause_pick h l =
    match UFSet.rchoose h l with
    | A a -> Dump.msg None (Some(fun p->p "Random focus on %a" IForm.print_in_fmt a)) None; a
    | _   -> let a = UFSet.choose l in
	     Dump.msg None (Some(fun p->p "Random problematic focus on %a" IForm.print_in_fmt a)) None; a


  (**************************************************)
  (* Not so last resort: picking decision literals *)

  (* Backjump stuff *)

  exception Backjump of bool*IForm.t*ASet.t*FSet.t

  let backjump cut = function
    | Provable(Seq.EntUF(aset,delta,formP,formPSaved,_,_),_,_)
        when FSet.mem cut delta ->
       (* (print_endline(Dump.toString(fun p->p *)
       (*   "Raising exception on %a\nwhere atms=%a and delta=%a" IForm.print_in_fmt cut Seq.print_in_fmt seq FSet.print_in_fmt delta)); *)
        raise (Backjump(false,cut,aset,FSet.union formP formPSaved))
    | _ ->
       (* (print_endline(Dump.toString(fun p->p *)
       (*   "Not raising exception on %a" IForm.print_in_fmt cut))); *)
      ()

  (* VSIDS heuristics *)

  module Dest = struct
    type keys = LitF.t
    let kcompare = LitF.compare
    type values = float
    type infos = (LitF.t*float) option
    let info_build = {
      empty_info  = None;
      leaf_info   = (fun l score -> Some(l,score));
      branch_info = (fun x1 x2 -> match x1,x2 with
      | None,_ -> failwith "Bad1"
      | _,None -> failwith "Bad2"
      | Some(_,v1),Some(_,v2)-> if v1>v2 then x1 else x2
      )
    }
    let treeHCons = None
  end

  module Scores = PATMap.Make(Dest)(TypesFromHConsed(LitF))
  let scores = ref Scores.empty
  let bump = ref 1.
  let since_last = ref 1

  let bump_lit lit =
    Scores.add lit (function
    | None -> !bump
    | Some score -> score +. !bump)

  let bump_set aset =
    scores := UASet.fold bump_lit aset !scores

  let divide_all v =
    scores := Scores.map (fun _ score -> score /. v) !scores;
    bump   := !bump /. v

  let factor = 2.**100.

  let bumpNdivide aset =
    bump_set aset;
    incr since_last;
    if !since_last > 100
    then 
      (Dump.msg None (Some(fun p-> p "Dividing all, bump being %f\n" !bump)) None;
       divide_all factor;
       since_last := 1)
    else bump := 2. *. !bump

  let tomem a = match Me.tomem a with
    | Some(aset,_) ->
       Dump.msg None (Some(fun p-> p "Bumping %a with %f\n" UASet.print_in_fmt aset !bump)) None;
      bumpNdivide aset
    | None -> ()
       
  (* Picks an atom from tset to branch on it *)

  let decide tset = 
    if decide_cut && not (UASet.is_empty tset) then
      match Scores.info(Scores.inter_poly (fun _ () x -> x) tset !scores) with
      | Some(lit,_) ->
        (* if UASet.mem lit atms then failwith "Chosen lit in atms"; *)
        (* if UASet.mem (LitF.negation lit) atms then failwith "Chosen nlit in atms"; *)
         count.(3)<-count.(3)+1;
        let cut =
          let nlit = LitF.negation lit in
          IForm.lit (if UASet.mem nlit !last_model then lit else nlit)
        in
        Some cut
      | None -> None
    else
      None



  (**************************)
  (* Unit propagation stuff *)

  (* Picks n lits out of set *)

  let rec picklits set n =
    if n=0 || UASet.is_empty set then []
    else
      let lit,set' = UASet.next set in 
      lit::(picklits set' (n-1))

  let close_branch form ad =
    (* Dump.msg None (Some (fun p->p "Yes %a" IForm.print_in_fmt a)) None; *)
    address := Yes (data_of_ad ad).i;
    count.(1)<-count.(1)+1;
    let now = count.(0) in
    let myaccept a =
      if isProvable a && count.(0)==now
      then address:=No
      else failwith "Expected Success2"
    in
    Some(Focus(form,branch_one ad,myaccept,fun ()->failwith "Expected success")),
    ad

  let propagate form ad =
    count.(2)<-count.(2)+1;
    (* print_string ("propagating"^Dump.stringOf IForm.print_in_fmt form); *)
    Some(Focus(form,branch_one ad,accept,fNone)),
    ad

  let rec up_next atms state =
    match UP.next atms state with
    | Some(_,None), state -> up_next atms state
    | Some(f,Some(c,_)), state -> Some(f,c), state
    | None, state -> None, state


  let update seq ad =
    let atms, formulae = Seq.forPlugin seq in
    let data       = data_of_ad ad in
    (* Computing the literals that may be watched and just got determined *)
    let newatoms   = UASet.inter atms data.remlits in
    let fixed      = UASet.union newatoms (UASet.negations newatoms) in
    let remlits    = UASet.diff data.remlits fixed in
    (* Fixing those newly fixed literals in the 2-watched lits table *)
    let state      = UASet.fold UP.fix fixed data.up_state in
    (* First, we look at whether the 2-watched technique yields a propagation or conflict *)
    let ans, state = up_next atms state in
    let data       = {data with remlits = remlits; up_state = state } in
    match ans with
    | Some(form,c) when UASet.is_empty c ->
       (* 2-watched technique yield conflict, we place
          the focus on corresponding clause *)
       close_branch form (ad_up ad data)
    | Some(form,_) ->
       (* 2-watched technique yield propagation, we place
          the focus on corresponding clause *)
       (* print_string("Propagating "^Dump.stringOf IForm.print_in_fmt form^"\nfrom "^Dump.stringOf UASet.print_in_fmt atms^"\n"); *)
       propagate form (ad_up ad data)
    | None ->
       (* 2-watched technique didn't yield propagation or conflict, we
          run the following function on the new data, declaring we
          haven't checked if we have new formulae *)
       let rec treat_stack data form_checked = match data.stack with

         (* Stack of formulae to be treated, including the latest
            additions, is empty: we run 2-watched lits *)
         | [] when form_checked ->
            let ans, state = up_next atms data.up_state in
            let ad = ad_up ad {data with up_state = state} in
            begin
              match ans with
              | Some(form,c) when UASet.is_empty c -> close_branch form ad
              | Some(form,_)                       -> propagate form ad
              | None                               -> None, ad
            end

         (* Stack of formulae to be treated is empty, but we haven't
            checked the latest additions: we check them *)
         | [] ->
            (* First, we identify the new formulae *)
            let newforms = UFSet.diff formulae data.knownforms in

            (* We stack up all new forms that contain literals *)
            let stack_newform form stack =
              if not(UF.fset form)
              then form::stack
              else stack
            in
            let stack = UFSet.fold stack_newform newforms [] in
            (* We start the analysis of the stack, declaring the check was made *)
            treat_stack { data with knownforms=formulae; stack = stack} true

         (* Stack of formulae to be treated is not empty *)
         | form::stack ->
            let aset  = UASet.diff (FormulaF.data form) atms in
            let naset = UASet.negations aset in
            (* First, we check if atms contains one of the clause's
               literals, in which case the clause is true, so useless *)
            if UASet.is_empty(UASet.inter atms naset)
            then 
              (* Literals of the clause that aren't already fixed *)
              let remlits() = UASet.union data.remlits (UASet.union aset naset) in 
              bump_set aset;
              (* print_string(Dump.stringOf IForm.print_in_fmt form^" "); *)
              match picklits aset 2 with
              | lit1::lit2::_ ->
                 (* print_string ("2-watched"^"\n"); *)
                 treat_stack { data with 
                   remlits = remlits();
                   stack = stack;
                   up_state = UP.addconstraint (form,Some(aset,naset)) lit1 lit2 data.up_state}
                   form_checked
              | [lit] -> 
                 (* print_string ("1-watched"^"\n"); *)
                 let data = { data with
                   i = count.(0);
                   restore_parity = (data_of_ad ad).restore_parity;
                   remlits  = remlits();
                   stack    = stack }
                 in
                 propagate form (ad_up ad data)
              | [] ->
                 (* print_string ("0-watched"^"\n"); *)
                close_branch form (ad_up ad data)
            else treat_stack {data with stack = stack } form_checked
       in
       treat_stack data false


  (****************************)
  (* Recursive function solve *)

  let rec solve_rec = function

	(* When we are notified of new focus point, we
	   - accept defeat if kernel has detected a loop and proposes to fail
	   - pass to the kernel an address for new focus point (count.(0)+1)
	   - our exit_function is always instructing kernel to memoise
	   result and accept it
	   - instruct the kernel to look for success in memoisation table
	   - accept the result of that search
	   - if almo flag is on, the search should be approximate, and
	   cut_series says what to do with approximate results
	   - if almo flag is off, the search is exact and no further
	   instruction is given to kernel
	*)

    | InsertCoin(Notify(seq,_,inloop,machine,ad))
      ->

      (* If kernel warns that no progress has been made, we accept defeat *)

      if inloop then solve_rec(machine(true,ad,accept,fNone))

      else
	(
	  (* if !Flags.debug>0 && (count.(0) mod Flags.every.(7) ==0) then report(); *)
	  count.(0)<-count.(0)+1;

	  let adOr = branch OrNode (ad []) in

          let andW b ad = 
            let data = data_of_ad ad in
            let newdata = { data with mem_first=true } in
            let datal,datar = if b then newdata,data else data,newdata in
            and_branch ad datal datar
          in
          
          (* We start building the next action to perform:
             First, we look at whether we can do unit propagations
             Second, we test whether a tabled proof can be pasted there
          *)
	  let action, adOr =
            if (data_of_ad adOr).mem_first
            then
              ((* print_endline "going for mem first"; *)
               match Me.search4provableNact seq (branch_one adOr) fNone () with
               | Some(Propose _) as a -> a, adOr
               | _ -> failwith "didn't find it")
            else
              let action, adOr = update seq adOr in
	      (match action with
	      | Some _ -> action
	      | None   -> Me.search4provableNact seq (andW false adOr) fNone ()),
              adOr
          in

          let restart action = 
            (if restart_strategy#is_enabled then
                (match action with
                | Some (Propose a) ->
                   let count = Me.get_usage_stats4provable a in
                   if count >= restart_strategy#next then (
                     Dump.Plugin.incr_count 10;
                     Me.reset_stats4provable a;
                     raise (Restarts.Restart (model(sequent a)))
                   )
                | _ -> ()))
          in

	  match action with
	  | Some _ -> begin
            restart action;
            try solve_rec (machine (true,el_wrap adOr,tomem,fun()->action))
            with
              WrongInstructionException _ ->
                Dump.Kernel.toPlugin();
	        solve_rec (machine (true, el_wrap adOr, tomem, fNone))
          end
	  | None -> begin
            let modelb4, formsb4 = match seq with
              | Seq.EntUF(atms,_,formP,formPSaved,_,_) -> atms, FSet.union formP formPSaved
              | _ -> failwith "Sequent is supposed to be unfocussed at the point of decision"
            in
            let data = data_of_ad adOr in
            (* Our next action will be determined by decide *)
            match decide data.remlits with
            | Some f -> begin
              (* print_endline(Dump.toString(fun p->p "I decide %a" IForm.print_in_fmt f)); *)
              try
                let newad = and_branch adOr { data with modelb4 = modelb4; formsb4 = formsb4 } data in
                let action() = Some(Cut(7, f, newad, backjump f, accept, fNone)) in
	        solve_rec (machine (true,el_wrap adOr,tomem,action))
                with
                | Backjump(false,cut,aset,fset) 
                    when (IForm.compare f cut==0
                          && ASet.subset aset (data_of_ad adOr).modelb4
                          && FSet.subset fset (data_of_ad adOr).formsb4)
                      -> raise (Backjump(true,cut,aset,fset))
                | Backjump(b,cut,aset,fset)
                    when (b || IForm.compare f cut==0)
                      && not (ASet.subset aset (data_of_ad adOr).modelb4
                              && FSet.subset fset (data_of_ad adOr).formsb4) ->
                   begin
                     (* print_endline (Dump.toString(fun p->p "cut= %a\n f= %a" IForm.print_in_fmt cut IForm.print_in_fmt f)); *)
                     Dump.Kernel.toPlugin();
                     let newad = andW true adOr in
                     let action() = Some(Cut(7, cut, newad, accept,
                                             (* (function *)
                                             (* | Provable(seq,_,_) as ans -> *)
                                                (* print_endline (Dump.toString(fun p->p "Proved again\n%a" Seq.print_in_fmt seq)); *)
                                             (*    accept ans *)
                                             (* | _ -> failwith "Dumb shit"), *)
                                             accept,fNone)) in
                     (* print_endline(Dump.toString(fun p->p "I re-decide %a" IForm.print_in_fmt cut)); *)
                     solve_rec (machine (true,el_wrap adOr,tomem,action))
                   end
            end
            | _ -> solve_rec (machine (true,el_wrap adOr,tomem,fun()->action))
          end
	)
	  
    (* When we are asked for focus: *)
    (* We
       - start searching whether a bigger sequent doesn't already
       have a counter-model
       - accept the result of that search
       - A(...) indicates that we look for an exact inclusion of our
       sequent in a bigger sequent
       - in case we have not found happiness in memoisation table,
       run focus_pick to determine action to perform, passing it
       the set of atoms, the set of formulae on which focus is
       allowed, and the address of the current focus point
    *)

    | InsertCoin(AskFocus(seq,_,l,_,false,machine,ad)) (* when UFSet.is_empty l *)
      -> solve_rec(machine(Me.search4notprovableNact seq (fun()->ConsistencyCheck(ad,accept,fNone))))

    (* | InsertCoin(AskFocus(seq,_,l,_,_,machine,ad))  when UFSet.is_empty l   *)
    (*     -> solve_rec(machine(Me.search4notprovableNact seq (fun()->ConsistencyCheck(ad,accept,fNone))))  *)

    (* If there is no more positive formulae to place the focus on,
       we restore the formulae on which we already placed focus *)

    | InsertCoin(AskFocus(_,_,l,true,_,machine,ad)) when FSet.is_empty l
	->
      let a = ad[] in
      let newad = el_wrap(ad_up a { (data_of_ad a) with restore_parity = not (data_of_ad a).restore_parity})
      in
      let next_action = if (data_of_ad a).restore_parity
        then (Format.printf "Restoring but next thing is move\n%!";
              (fun ()->Some(Get(false,true,fNone))))
        else fNone in
      solve_rec(machine(Restore(newad,accept,next_action)))

    (* | InsertCoin(AskFocus(_,_,l,true,_,machine,olda)) when UFSet.is_empty l *)
    (*   -> solve_rec (machine(Restore(olda,accept,fNone))) *)


    | InsertCoin(AskFocus(seq,_,l,_,_,machine,ad))
      -> let atms = model seq in
         let a = ad[] in
         let alternative()= Focus(clause_pick atms (FSet.forPlugin l),branch_one a,accept,fNone)
         in
	 solve_rec(machine(Me.search4notprovableNact seq alternative))


    (* When we are asked a side, we always go for the left first *)

    | InsertCoin(AskSide(seq,_,machine,ad)) -> solve_rec (machine(true,branch_two(branch OrNode (ad[]))))

    (* When kernel has explored all branches and proposes to go
       backwards to close remaining branches, we give it the green
       light *)

    | InsertCoin(Stop(b1,b2, machine)) ->  Format.printf "Reaching the END\n%!"; 
      solve_rec (machine ())

    (* When the kernel gives us a final answer, we return it and clear all the caches *)

    | Jackpot ans                    -> report(); address:=No;
      restart_strategy#reset() ; Me.clear(); UASet.clear(); UFSet.clear();LitF.clear();Dump.Plugin.clear();
      for i=0 to Array.length count-1 do count.(i) <- 0 done;
      ans

    (* solve_restart handles restarts, launching solve_rec once, 
       then re-launching it every time a Restart exception is raised. *)
  let rec solve input =
    try 
      solve_rec input
    with Restarts.Restart lits -> 
      address:=No;
      last_model := lits;
      restart_strategy#increment ();
      Dump.Kernel.toPlugin ();
      Dump.Kernel.reset_branches ();
      (* if !Flags.debug>0 then *)
      print_endline (Printf.sprintf "Restarting (next restart: %d)" restart_strategy#next);
      solve input

end
