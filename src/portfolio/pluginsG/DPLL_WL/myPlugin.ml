open General
open Kernel
open Prop

open Interfaces_theory
open Literals
open Formulae
open Interfaces_plugin
open Patricia_interfaces
open Patricia
open Sums

open PluginsG_tools.Addressing

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

  type my_data = {i: int; remlits : UASet.t; restore_parity : bool}
  type data       = my_data addressing
  let initial_data _ = ad_init {i = 0; remlits = UASet.empty; restore_parity = false}
  let address     = ref No

  module Restarts = PluginsG_tools.RestartStrategies.RestartStrategies(UASet)
  let restart_strategy = Restarts.getbyname !Flags.restarts_strategy !Flags.restarts_p1 !Flags.restarts_p2
                                            
  (* We record a stack of clauses that will definitely do a Unit Propagate *)
  let stack   = ref []
  let priority_lits = ref (Queue.create ())

  let report i =
    print_endline("   Plugin's report (DPLL_WL):");
    print_endline(string_of_int count.(0)^" notifies, "^
		    string_of_int count.(1)^" Backtrack, "^
		      string_of_int count.(2)^" Unit propagate, "^
		        string_of_int count.(3)^" Decide, "^
		          string_of_int (Dump.Plugin.read_count 7)^" Memo backtrack, "^
		            string_of_int (Dump.Plugin.read_count 8)^" Memo UP "); 
    Me.report();
    print_endline ""

  (* We now implement the datastructures for the 2-watched literals
       technique.

       Each clause watches 2 of its literals.
       It is convenient, given a literal, to get all the clauses that
       are watching it and, for each of those clauses, to get the
       other literal that the clause watches.

       This is implemented as follows: 

       In a hashtable, we map every literal to a (persistent) map,
       whose keys are clauses and whose values are literals *)

  (* The following structure implements the above persistent map *)

  module CSetWatched = struct
    type keys        = UFSet.UT.keys
    let kcompare     = UFSet.UT.compare
    type values      = LitF.t
    let vcompare     = LitF.compare
    type infos       = unit
    let info_build   = empty_info_build
    let treeHCons    = None
  end

  module CSet = PATMap.Make(CSetWatched)(UFSet.UT)

  (* We now create the hashtable of watched literals *)

  module H = Hashtbl.Make(PluginsG_tools.Utils.PHCons_ext(LitF))
  let watched = H.create 5003

  (* Here we can check that the table is well-formed. This function
     is not actually used. Just there for debugging *)

  let tablecheck() =
    let c =ref 0 in
    H.iter (fun lit set ->
        CSet.iter (fun clause newlit->
	    incr c;
	    if not (H.mem watched newlit) then failwith "fad1";
	    let l' = H.find watched newlit in
	    if not(CSet.mem clause l') then failwith "fad2";
	    let x=CSet.find clause l' in
	    if not(LitF.compare x lit==0) then failwith "fad3")
	  set
      ) watched

  (* Boolean to record whether we need to initialise the table of
       watched literals. At first, is true. *)

  let is_init = ref true

  (* Assume setatms is a subset of set (sets of literals).
       Pick n literals out of set (or as many as you can),
       with priority given to those literals in setatms. *)

  let rec pickaux set setatms n =
    if n=0 then []
    else
      if UASet.is_empty setatms
      then 
	if UASet.is_empty set then []
	else let (lit,set') = UASet.next set in 
	     lit::(pickaux set' UASet.empty (n-1))
      else let (lit,setatms') = UASet.next setatms in
	   if not(UASet.mem lit set) then failwith("pickaux: 2nd arg not subset of 1st arg");
	   lit::(pickaux (UASet.remove lit set) setatms' (n-1))

  (* Pick n literals (or as many as you can) out of formula (which
       represents a clause), if possible not in atms *)

  let picklit formula atms n = 
    let l = FormulaF.data formula in
    pickaux l (UASet.diff l atms) n

  (* Adds formula as one of the clauses watching lit, the other
       literal watched by formula being lit2 *)

  let addlit lit formula lit2 = 
    if not (H.mem watched lit) then H.add watched lit CSet.empty;
    let l' = H.find watched lit in
    H.replace watched lit (CSet.add formula (fun _ -> lit2) l')

  (* Function to initialise the table of watched literals:

       Go through every clause in cset, and for each clause, try to
       pick 2 of its literals to watch (if possible not in atms);
       if successful, add the 2 corresponding entries in the hashtable.

       Whilst doing that, collect in alllits all of the literals you
       see. Finally, if the decide_cut flag is on, output all of
       those which do not appear in atms and whose negations do not
       appear either.  *)

  let initialise atms cset = 
    let alllits = ref UASet.empty in
    UFSet.iter 
      (fun formula -> 
	(
	  (match picklit formula atms 2 with
	   | lit1::lit2::_ when not (UF.fset formula)->
              (addlit lit1 formula lit2 ;
	       addlit lit2 formula lit1 )
	   | _ -> ());
	  if decide_cut then
            alllits := UASet.union !alllits (FormulaF.data formula)
	)
      )
      cset;
    if decide_cut then 
      (Dump.print ["dpll_wl",1] (fun p->p "%i atoms and negations present in non-unit clauses" (UASet.cardinal !alllits));
       let onoatms = UASet.union atms (UASet.negations atms) in
       let res = UASet.diff !alllits onoatms in
       UASet.union res (UASet.negations res)
      )
    else UASet.empty

  (* Function treating the set t of clauses that are watching literal
       lit, while the current model is atms.
       outputs a pair: 
       - clause for backtrack, if there is one
       - new set of clauses watching literal lit
   *)

  let treat atms t =
    let rec aux t = match CSet.reveal t with
      | Empty           -> (None,t)
      | Leaf(j,x)       ->
	 (match UASet.sub true (FormulaF.data j) atms None with

	  | Yes _   ->
	     (* Clause j is allowing backtrack;
	       we stop investigating the table of watched literals and return j *)
	     (Some j,t)

	  | Almost n when not (UASet.mem (LitF.negation n) atms) ->
	     (* Clause j is allowing Unit Propagation;
	       we stack it, and leave the table of watched literals unchanged *)
	     stack:= (j,n,false)::!stack;
	     (None,t)

	  | Almost n -> (None,t)

	  | _ ->
	     (* Clause j allows neither backtrack nor unit propagate;
	       we need to change the watched literal lit *)
	     (* Pick a new literal to be watched in l *)
	     let tobcf = UASet.remove x (FormulaF.data j) in
	     (match pickaux tobcf (UASet.diff tobcf atms) 1 with
	      | [newlit] ->
	         (* if (x=newlit)   then failwith "x=newlit"; *)
	         (* if (lit=newlit) then failwith "lit=newlit"; *)
	         (* if (x=lit)      then failwith "x=lit"; *)
	         (* Updating entry of other watched literal x:
	       lit -> newlit *)
	         if not (H.mem watched x) then failwith "fad";
	         (let watchingx = H.find watched x in
	          H.replace watched x (CSet.add j (fun _ -> newlit) (CSet.remove j watchingx)));
	         (* Updating entry of new literal newlit *)
	         if not (H.mem watched newlit) then H.add watched newlit CSet.empty;
	         let watchingnewlit = H.find watched newlit in
	         H.replace watched newlit (CSet.add j (fun _ -> x) watchingnewlit);
	         (* Final output: no backtrack clause, clause j removed from entry of lit *)
	         (None,CSet.empty)
	      | _ -> failwith "Disgrace"
	     )
	 )  

      | Branch(_,_,l,r) -> 
	 (match aux r with
	  | None,   r' -> let v, l' = aux l in (v,CSet.union (fun a _ -> a) l' r')
	  | Some c, r' -> (Some c,CSet.union (fun a _ -> a) l r')
	 )
    in aux t

  (* TODO:
       - search for presence of negative (eliminate clauses that are true)
       - write treat in CPS
       - integrate watched lit for learnt clauses
   *)

  let update atms ad =
    let newatoms = UASet.inter atms (data_of_ad ad).remlits in
    let tset' = UASet.diff (data_of_ad ad).remlits (UASet.union newatoms (UASet.negations newatoms)) in
    let newad = ad_up ad { i= count.(0); remlits = tset'; restore_parity = (data_of_ad ad).restore_parity } in
    let aux lit = function
      | None when H.mem watched lit ->
	 let l = H.find watched lit in
	 let answer, l' = treat atms l in
         H.replace watched lit l';
         answer
      | a -> a
    in
    match UASet.fold aux newatoms None with
    | Some a -> Dump.print ["dpll_wl",2] (fun p->
                    p "Yes %a" (fun fmt -> IForm.print_in_fmt fmt) a);
                address:=Yes((data_of_ad ad).i);
                count.(1)<-count.(1)+1;
                let now = count.(0) in
                let myaccept a = 
                  if isProvable a && count.(0)==now
                  then address:=No
                  else failwith "Expected Success2"
                in
                stack:=[];
                (Some(Focus(a,branch_one newad,myaccept,fun ()->failwith "Expected success")),newad)
    | None -> (None,newad)


  exception Nonempty_intersection of UASet.t

  let pick_lit_from tset =
    let select_pool lemma_litterals =
      let inter = UASet.inter lemma_litterals tset in
      if not (UASet.is_empty inter) then raise (Nonempty_intersection inter) in
    let pool = 
      try Queue.iter select_pool !priority_lits; tset
      with Nonempty_intersection inter -> inter
    in UASet.choose pool

  (* Picks an atom from tset to branch on it *)

  let decide atms adO tset () = 
    if decide_cut && not (UASet.is_empty tset) then
      (let lit = pick_lit_from tset in
       if UASet.mem lit atms then failwith "Chosen lit in atms";
       if UASet.mem (LitF.negation lit) atms then failwith "Chosen nlit in atms";
       count.(3)<-count.(3)+1;
       Some(Cut(7,IForm.lit lit,branch_one adO,accept,accept,fNone)))
    else
      None
	
  let clause_pick h l =
    if not(List.length !stack = 0) then failwith("pas []");
    match UFSet.rchoose h l with
    | Case1 a       -> Dump.print ["dpll_wl",2] (fun p->
                           p "Random focus on %a"
                             (fun fmt -> IForm.print_in_fmt fmt) a);
                       a
    | _         -> let a = UFSet.choose l in
	           Dump.print ["dpll_wl",2] (fun p->
                       p "Random problematic focus on %a"
                         (fun fmt -> IForm.print_in_fmt fmt) a);
                   a


  (* We look at the stack to see if there is any unit propagation left to do:
       if it contains a triple (f,n,_) then f is a clause available for
       UP and "not n" is the literal to be added to the model 
   *)

  let rec findaction atms ad alternative =
    match !stack with

    (* No UP to perform -> we fall back on the alternative action *)

    | []       -> alternative

    (* UP to perform in order to add "not n", checking that we don't already have it in atms *)

    | (f,n,_)::h when not (UASet.mem (LitF.negation n) atms)->
       (count.(2)<-count.(2)+1;
        stack:=h;
        fun()-> Some(Focus(f,branch_one ad,accept,fNone)))

    (* UP to perform in order to add "not n", but we already have it *)

    | (f,n,_)::h ->
       stack:=h;
       findaction atms ad alternative


  (* Recursive function solve *)

  let rec solve_rec_aux input = 
    match input with

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

           let a = ad [] in

           (* We test if this is the first ever call, in which case we initialise things *)
	   let adOr = 
	     branch OrNode 
               (if !is_init then
		  let (atms,cset)= Seq.forPlugin seq in 
		  is_init:=false;
                  ad_up a { i = (data_of_ad a).i; remlits = initialise atms cset; restore_parity = (data_of_ad a).restore_parity}
	        else a) in

	   let atms = model seq in

           (* We update our non-persistent data structures *)
	   let (action,adOr) = update atms adOr in
	   let tset' = (data_of_ad adOr).remlits in

           (* We start building the next action to perform:
                 first, we shall test if a tabled proof can be pasted there, otherwise our next action will be determined by decide *)

	   let alternative = 
             Me.search4provableNact seq (branch_one adOr) (decide atms adOr tset')
	   in
           
           let alternative_restart () =
             let out = alternative () in
             if restart_strategy#is_enabled then
               (match out with
                | Some (Propose a) ->
                   let count = Me.get_usage_stats4provable a in
                   if count >= restart_strategy#next then (
                     Dump.Plugin.incr_count 10;
                     Me.reset_stats4provable a;
                     raise (Restarts.Restart (model(sequent a)))
                   )
                | _ -> ());
             out
           in
           try
	     solve_rec (machine (true,
			         el_wrap adOr,
			         (fun a-> let _ = Me.tomem a in ()),
			         match action with
			         | Some action as saction -> (* print_endline "Found an action!" *)(fun()->saction)
			         | None        -> 
                                    (* (match !stack with *)
                                    (* | [] ->print_endline "Looking for action with empty stack!"; *)
                                    (* | _ -> print_endline "Looking for action with non-empty stack!"); *)
                                    findaction atms adOr alternative_restart))
           with
             WrongInstructionException _ ->
             Dump.Kernel.toPlugin();
	     solve_rec (machine (true,
	      	                 el_wrap adOr,
	      	                 (fun a-> let _ = Me.tomem a in ()),
                                 fNone)      )

	 )
	   
    (* When we are asked for focus: *)

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
       let next_action = if (data_of_ad a).restore_parity then (Format.printf "Restoring but next thing is move\n%!"; (fun ()->Some(Get(false,true,fNone)))) else fNone in
       solve_rec(machine(Restore(newad,accept,next_action)))

    (* | InsertCoin(AskFocus(_,_,l,true,_,machine,olda)) when UFSet.is_empty l *)
    (*   -> solve_rec (machine(Restore(olda,accept,fNone))) *)


    (* We
	   - start searching whether a bigger sequent doesn't already
	   have a counter-model
	   - accept the result of that search
	   - Case1(...) indicates that we look for an exact inclusion of our
	   sequent in a bigger sequent
	   - in case we have not found happiness in memoisation table,
	   run focus_pick to determine action to perform, passing it
	   the set of atoms, the set of formulae on which focus is
	   allowed, and the address of the current focus point
     *)

    | InsertCoin(AskFocus(seq,_,l,_,_,machine,ad))
      -> let atms = model seq in
         let a = ad[] in
         let alternative()= match findaction atms a fNone () with
           | Some action -> action
           | None -> Focus(clause_pick atms (FSet.forPlugin l),branch_one a,accept,fNone)
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

    | Jackpot ans                    -> report(); stack := []; is_init:=true; address:=No;
                                        H.clear watched; restart_strategy#reset() ; Me.clear(); UASet.clear(); UFSet.clear();LitF.clear();Dump.Plugin.clear();
                                        for i=0 to Array.length count-1 do count.(i) <- 0 done;
                                        ans

  and solve_rec input =
    (* begin *)
    (*   match input with *)
    (*   | InsertCoin(Notify(seq,_,inloop,machine,ad)) *)
    (*     -> Format.printf "Notify: %a%!" print_in_fmt_ad (ad[]) *)
    (*   | InsertCoin(AskFocus(seq,_,l,_,false,machine,ad)) when UFSet.is_empty l *)
    (*     -> Format.printf "AskFocus empty l + false: %a%!" print_in_fmt_ad (ad[]) *)
    (*   | InsertCoin(AskFocus(_,_,l,true,_,machine,ad)) when UFSet.is_empty l *)
    (*     -> Format.printf "AskFocus empty l + true: %a%!" print_in_fmt_ad (ad[]) *)
    (*   | InsertCoin(AskFocus(seq,_,l,_,_,machine,ad)) *)
    (*     -> Format.printf "AskFocus non-empty l: %a%!" print_in_fmt_ad (ad[]) *)
    (*   | InsertCoin(AskSide(seq,_,machine,ad)) *)
    (*     -> Format.printf "AskSide: %a%!" print_in_fmt_ad (ad[]) *)
    (*   | InsertCoin(Stop(b1,b2, machine)) *)
    (*     -> Format.printf "Reached end: %b %b %!" b1 b2 *)
    (*   | Jackpot ans *)
    (*     -> Format.printf "Jackpot%!" *)
    (* end; *)
    (* ignore (read_line ()); *)
    solve_rec_aux input


  let reset () =
    stack := [];
    is_init:=true;
    address:=No;
    H.clear watched

  (* solve_restart handles restarts, launching solve_rec once, 
       then re-launching it every time a Restart exception is raised. *)
  let rec solve input =
    try 
      solve_rec input
    with Restarts.Restart lits -> 
      reset();
      Queue.push lits !priority_lits;
      restart_strategy#increment ();
      Dump.Kernel.toPlugin ();
      Dump.Kernel.reset_branches ();
      (* if !Flags.debug>0 then *)
      print_endline (Printf.sprintf "Restarting (next restart: %d)" restart_strategy#next);
      solve input

end
