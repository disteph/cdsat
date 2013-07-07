open Lib
open Kernel

open Interfaces
open Formulae
open Patricia
open Sums

module GenPlugin(Atom: AtomType):(Plugins.Type with type literals = Atom.t) = struct
    
  type literals = Atom.t
  module DataStruct = DataStructures.Generate(Atom)
  module UASet = DataStruct.ASet
  module UF    = DataStruct.F
  module UFSet = DataStruct.FSet

  let count = [|0;0;0;0|]

  (* Flag decide whether we do
     - binary decide rules, using cut (decide_cut is true)
     - or random focus on remaining clauses (decide_cut is false)
  *)
  let decide_cut = true

    
  module Strategy(FE:FrontEndType with type litType     = literals
				  and  type formulaType = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t) = struct
    include FE
    include Common.Utils.FEext(FE)
    module Me = Common.Utils.Memo(Atom)(FE)(UFSet)(UASet)

    type data       = int*UASet.t
    let initial_data _ = (0,UASet.empty)
    let address     = ref No

    (* We record a stack of clauses that will definitely do a Unit Propagate *)
    let stack   = ref []

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


    (* The following structures implement the table of watched literals *)

    module CSetWatched = struct
      type keys        = UFSet.UT.keys
      let kcompare     = UF.compare
      type values      = literals
      let vcompare     = Atom.compare
      type infos       = unit
      let info_build   = empty_info_build
      let treeHCons    = true
    end

    module CSet = PATMap(CSetWatched)(UFSet.UT)

    module H = Hashtbl.Make(Atom)
    let watched = H.create 5003

    let tablecheck() =
      let c =ref 0 in
	H.iter (fun lit set ->
		  CSet.iter (fun clause newlit->
			       incr c;
			       if not (H.mem watched newlit) then failwith("fad1");
			       let l' = H.find watched newlit in
				 if not(CSet.mem clause l') then failwith("fad2");
				 let x=CSet.find clause l' in
				   if not(x=lit) then failwith("fad3"))
		    set
	       ) watched

    (* Boolean to record whether we have initialised the table of
       watched literals *)
    let is_init = ref true

    let rec pickaux set setatms n =
      if n=0 then []
      else
	if UASet.is_empty setatms
	then 
	  if UASet.is_empty set then []
	  else let (lit,set') = UASet.next set in 
	    lit::(pickaux set' UASet.empty (n-1))
	else let (lit,setatms') = UASet.next setatms in
	  if not(UASet.is_in lit set) then failwith("Found");
	  lit::(pickaux (UASet.remove lit set) setatms' (n-1))

    let picklit formula atms n = 
      let l = UF.aset formula in
      pickaux l (UASet.diff l atms) n

    let addlit lit formula lit2 = 
      if not (H.mem watched lit) then H.add watched lit CSet.empty;
      let l' = H.find watched lit in
	H.replace watched lit (CSet.add formula (fun _ -> lit2) l')

    (* Function to initialise  the table of watched literals *)

    let initialise atms cset = 
      let alllits = ref UASet.empty in
	UFSet.iter 
	  (fun formula -> 
	     (
	       (match picklit formula atms 2 with
		  | lit1::lit2::_ when not (UF.fset formula)-> (addlit lit1 formula lit2 ;
				                                addlit lit2 formula lit1 )
		  | _ -> ());
	       if decide_cut then
                 alllits := UASet.union !alllits (UF.aset formula)
	     )
	  )
	  cset;
	if decide_cut then 
	  (print_endline(string_of_int(UASet.cardinal !alllits)^" atoms and negations present in non-unit clauses");
	   let onoatms = UASet.union atms (UASet.negations atms) in
	     UASet.diff !alllits onoatms)
	else UASet.empty

    (* Function treating the set t of clauses that are watching literal
       lit, while the current model is atms.
       outputs a pair: 
       - clause for backtrack, if there is one
       - new set of clauses watching literal lit
    *)

    let treat atms lit t =
      let rec aux t = match CSet.reveal t with
	| CSet.Empty           -> (None,t)
	| CSet.Leaf(j,x)       ->
	  (match UASet.sub true (UF.aset j) atms None with
	  | Yes _   ->
			  (* Clause j is allowing backtrack;
			     we stop investigating the table of watched literals and return j *)
	    (Some j,t)
	  | Almost n when not (UASet.is_in (Atom.negation n) atms) ->
			  (* Clause j is allowing Unit Propagation;
			     we stack it, and leave the table of watched literals unchanged *)
	    stack:= (j,n,false)::!stack;
	    (None,t)
	  | Almost n -> (None,t)
	  | _ ->
	    (* Clause j allows neither backtrack nor unit propagate;
	       we need to change the watched literal lit *)
	    (* Pick a new literal to be watched in l *)
	    let tobcf = UASet.remove x (UF.aset j) in
	    (match pickaux tobcf (UASet.diff tobcf atms) 1 with
	    | [] -> failwith("Disgrace")
	    | newlit::_ ->
	      (* if (x=newlit)   then failwith("x=newlit"); *)
	      (* if (lit=newlit) then failwith("lit=newlit"); *)
	      (* if (x=lit)      then failwith("x=lit"); *)
	      (* Updating entry of other watched literal x:
		 lit -> newlit *)
	      if not (H.mem watched x) then failwith("fad");
	      (let watchingx = H.find watched x in
	       H.replace watched x (CSet.add j (fun _ -> newlit) (CSet.remove j watchingx)));
	      (* Updating entry of new literal newlit *)
	      if not (H.mem watched newlit) then H.add watched newlit CSet.empty;
	      let watchingnewlit = H.find watched newlit in
	      H.replace watched newlit (CSet.add j (fun _ -> x) watchingnewlit);
	      (* Final output: no backtrack clause, clause j removed from entry of lit *)
	      (None,CSet.empty)
	    )
	  )  
	| CSet.Branch(p,m,l,r) -> 
	    (match aux r with
	       | (None,r')   -> let (v,l') = aux l in (v,CSet.union (fun a _ -> a) l' r')
	       | (Some c,r') -> (Some c,CSet.union (fun a _ -> a) l r')
	    )
      in aux t

    (* TODO:
       - search for presence of negative (eliminate clauses that are true)
       - write treat in CPS
       - integrate watched lit for learnt clauses
    *)

    let update atms (olda,tset) = match UASet.latest atms with
      | Some lit ->
	  let tset' = 
	    if decide_cut then 
	      (let nolit = Atom.negation lit in
	       let tsettmp = if UASet.is_in nolit tset then UASet.remove nolit tset else tset in
		 if UASet.is_in lit tsettmp then UASet.remove lit tsettmp else tsettmp)
	    else tset
	  in
	    if H.mem watched lit then
	      let l = H.find watched lit in
	      let (answer,l') = treat atms lit l in
		(H.replace watched lit l'; 
		 match answer with
		   | Some a -> if !Flags.debug>1 then print_endline("Yes "^Form.toString a);
		       address:=Yes(olda);
		       count.(1)<-count.(1)+1;
		       let now = count.(0) in
		       let myaccept a = 
                         if (isSuccess a&& count.(0)==now)
                         then (address:=No;Accept)
                         else failwith "Expected Success"
		       in
			 stack:=[];
			 (Some(Focus(a,myaccept,fun ()->failwith("Expected success"))),tset')
		   | None -> (None,tset')
		)
	    else (None,tset')
      | _ -> (None,tset)



    let decide atms tset () = 
      if decide_cut && not (UASet.is_empty tset) then
	(let lit = UASet.choose tset in
	   if UASet.is_in lit atms then failwith("Chosen lit in atms");
	   if UASet.is_in (Atom.negation lit) atms then failwith("Chosen nlit in atms");
	   Some(Cut(7,UF.build (Lit lit),accept,accept,fNone)))
      else
	None
	  
    let clause_pick h l =
      if not(!stack=[]) then failwith("pas []");
      count.(3)<-count.(3)+1;
      match UFSet.rchoose h l with
	| A a       -> if !Flags.debug>1 then print_endline("Random focus on "^Form.toString a); a
	| _         -> let a = UFSet.choose l in
	    if !Flags.debug>1 then print_endline("Random problematic focus on "^Form.toString a); a


    let rec findaction atms alternative =
      match !stack with
	| []       -> alternative
	| (f,n,_)::h when not (UASet.is_in (Atom.negation n) atms)->
	    (count.(2)<-count.(2)+1;
	     stack:=h;
	     fun()->Some(Focus(f,accept,fNone)))
	| (f,n,_)::h ->
	    stack:=h;
	    findaction atms alternative

    let rec solve input = (*print_time report;*)
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

	| Fake(Notify(seq,inloop,machine,(olda,tset)))
	  ->(* (match !address with *)
	    (*    | Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp) *)
	    (*    | Yes(exp) -> failwith("Yes not expected") *)
	  (*    | _ -> address:=No); *)
	  if inloop then solve(machine(true,(count.(0),tset),accept,fNone))
	  else
	    (
	      if !Flags.debug>0 && (count.(0) mod Flags.every.(7) ==0) then report();
	      count.(0)<-count.(0)+1;
	      let tsetnew = 
		(if !is_init then
		    let (atms,cset)= Seq.simplify seq in 
		    is_init:=false; initialise atms cset
		 else tset) in
	      let atms = model seq in
	      let (action,tset') = update atms (olda,tsetnew) in
	      let alternative = Me.search4successNact seq (decide atms tset')
	      in
	      (if !Flags.debug >1 then 
		  (let u,u' = UASet.cardinal tset,UASet.cardinal tset' in
		   if u'>0 then print_endline(string_of_int u')
		   else if u>0 then report()));
	      solve (machine (true,
			      (count.(0),tset'),
			      Me.memAccept,
			      match action with  
			      | Some action as saction -> (fun()->saction)
			      | None        -> findaction atms alternative
	      )
	      )
	    )
		
	(* When we are asked for focus: *)

	(* If there is no more positive formulae to place the focus on,
	   we restore the formulae on which we already placed focus *)

	| Fake(AskFocus(_,l,true,_,machine,_)) when UFSet.is_empty l
	    -> solve(machine(Restore fNone))

	| Fake(AskFocus(seq,l,false,_,machine,_)) when UFSet.is_empty l
	    -> solve(machine(Me.search4failureNact seq (fun()->ConsistencyCheck(accept,fNone))))

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

	| Fake(AskFocus(seq,l,_,_,machine,(olda,tset)))
	  -> let atms = model seq in
             let alternative()= match findaction atms fNone () with
               | Some action -> action
               | None -> Focus(clause_pick atms l,accept,fNone)
             in
	     solve(machine(Me.search4failureNact seq alternative))


	(* When we are asked a side, we always go for the left first *)

	| Fake(AskSide(seq,machine,_)) -> solve (machine true)

	(* When kernel has explored all branches and proposes to go
	   backwards to close remaining branches, we give it the green
	   light *)

	| Fake(Stop(b1,b2, machine))   -> solve (machine ())

	(* When the kernel gives us a final answer, we return it and clear all the caches *)

	| Local ans                    -> report(); stack := []; is_init:=true; address:=No;
	    H.clear watched; Me.clear(); UF.clear(); UASet.clear(); UFSet.clear();Atom.clear();Dump.Plugin.clear();
	    for i=0 to Array.length count-1 do count.(i) <- 0 done;
	    ans

  end

end
