open Lib
open Kernel
open Interfaces
open Formulae
open Patricia
open Sums
open MyPatASet
open MyDPLLStructures


module GenPlugin(MyTheory: TheoryType):(Plugin.Type with type literals = MyTheory.Atom.t) = struct
    
  type literals = MyTheory.Atom.t

  module UASet = MyPatA(MyTheory.Atom)
  module UF    = MyDPLLForm(MyTheory.Atom)
  module UFSet = MyDPLLFSet(MyTheory.Atom)

  let count = [|0;0;0;0;0;0;0;0;0;0|]

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

    type data       = int*UASet.t
    let initial_data= (0,UASet.empty)
    let address     = ref No

    let fNone () = None

    module Me = Memo(UFSet)(UASet)
    module PF = Formulae.PrintableFormula(MyTheory.Atom)(UF)

    (* We record a stack of clauses that will definitely do a Unit Propagate *)
    let stack   = ref []

    let report i =
      Me.report();
      print_endline("   Plugin's report:");
      print_endline(string_of_int count.(0)^" notifies, "^
		      string_of_int count.(1)^" Backtrack, "^
		      string_of_int count.(2)^" Unit propagate, "^
		      string_of_int count.(3)^" Decide, "^
		      string_of_int count.(4)^" Memo backtrack, "^
		      string_of_int count.(5)^" Memo UP "); 
      print_endline ""


    (* The following structures implement the table of watched literals *)

    module CSetWatched = struct
      type keys        = UFSet.UT.keys
      let kcompare     = UFSet.UT.compare
      type values      = literals
      let vcompare     = MyTheory.Atom.compare
      type infos       = unit
      let info_build   = empty_info_build
      let treeHCons    = true
    end

    module CSet = PATMap(CSetWatched)(UFSet.UT)

    module H = Hashtbl.Make(MyTheory.Atom)
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
	then let (lit,set') = UASet.next set in 
	  lit::(pickaux set' UASet.empty (n-1))
	else let (lit,setatms') = UASet.next setatms in
	  if not(UASet.is_in lit set) then failwith("Found");
	  lit::(pickaux (UASet.remove lit set) setatms' (n-1))

    let picklit formula atms n = match UF.aset formula with
      | None   -> failwith("Trying to pick watched literals from something that's not a clause")
      | Some l -> pickaux l (UASet.diff l atms) n

    let addlit lit formula lit2 = 
      if not (H.mem watched lit) then H.add watched lit CSet.empty;
      let l' = H.find watched lit in
	H.replace watched lit (CSet.add (fun a _ -> a) formula lit2 l')

    (* Function to initialise  the table of watched literals *)

    let initialise atms cset = 
      let alllits = ref UASet.empty in
	UFSet.iter 
	  (fun formula -> 
	     (
	       (match picklit formula atms 2 with
		  | lit1::lit2::_ -> (addlit lit1 formula lit2 ;
				      addlit lit2 formula lit1 )
		  | _ -> ());
	       if decide_cut then
		 match UF.aset formula with
		   | None   -> failwith("Trying to get literals from something that's not a clause")
		   | Some l -> alllits := UASet.union !alllits l
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
	    (match UF.aset j with
	       | None   -> failwith("Should find a set of atoms")
	       | Some l ->
		   (match UASet.sub true l atms None with
		      | Yes _    ->
			  (* Clause j is allowing backtrack;
			     we stop investigating the table of watched literals and return j *)
			  (Some j,t)
		      | Almost n when not (UASet.is_in (MyTheory.Atom.negation n) atms) ->
			  (* Clause j is allowing Unit Propagation;
			     we stack it, and leave the table of watched literals unchanged *)
			  stack:= (j,n,false)::!stack;
			  (None,t)
		      | Almost n -> (None,t)
		      | _ ->
			  (* Clause j allows neither backtrack nor unit propagate;
			     we need to change the watched literal lit *)
			  (* Pick a new literal to be watched in l *)
			  let tobcf = UASet.remove x l in
			    (match pickaux tobcf (UASet.diff tobcf atms) 1 with
			       | [] -> failwith("Disgrace")
			       | newlit::_ ->
				   if (x=newlit) then failwith("x=newlit");
				   if (lit=newlit) then failwith("lit=newlit");
				   if (x=lit) then failwith("x=lit");
				   (* Updating entry of other watched literal x:
				      lit -> newlit *)
				   if not (H.mem watched x) then failwith("fad");
				   (let watchingx = H.find watched x in
				      H.replace watched x (CSet.add (fun a _ -> a) j newlit (CSet.remove j watchingx)));
				   (* Updating entry of new literal newlit *)
				   if not (H.mem watched newlit) then H.add watched newlit CSet.empty;
				   let watchingnewlit = H.find watched newlit in
				     H.replace watched newlit (CSet.add (fun a _ -> a) j x watchingnewlit);
				     (* Final output: no backtrack clause, clause j removed from entry of lit *)
				     (None,CSet.empty)
			    )
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
	      (let nolit = MyTheory.Atom.negation lit in
	       let tsettmp = if UASet.is_in nolit tset then UASet.remove nolit tset else tset in
		 if UASet.is_in lit tsettmp then UASet.remove lit tsettmp else tsettmp)
	    else tset
	  in
	    if H.mem watched lit then
	      let l = H.find watched lit in
	      let (answer,l') = treat atms lit l in
		(H.replace watched lit l'; 
		 match answer with
		   | Some a -> if !Flags.debug>1 then print_endline("Yes "^PF.toString a);
		       address:=Yes(olda);
		       count.(1)<-count.(1)+1;
		       let now = count.(0) in
		       let myaccept = function 
			 | Local(Success _) when count.(0)==now ->address:=No;Accept
			 | _-> failwith "Expected Success"
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
	   if UASet.is_in (MyTheory.Atom.negation lit) atms then failwith("Chosen nlit in atms");
	   Some(Cut(7,UF.build (Lit lit),accept,accept,fNone)))
      else
	None
	  
    let clause_pick h l () =
      if not(!stack=[]) then failwith("pas []");
      count.(3)<-count.(3)+1;
      match UFSet.rchoose h l with
	| A a       -> if !Flags.debug>1 then print_endline("Random focus on "^PF.toString a);
	    Some(Focus(a,accept,fNone))
	| _         -> let a = UFSet.choose l in
	    if !Flags.debug>1 then print_endline("Random problematic focus on "^PF.toString a);
	    Some(Focus(a,accept,fNone))

    let cut_series alternative (a,f)  () =
      if UASet.is_empty a then
	if UFSet.is_empty f then
	  alternative()
	else let (toCut,f')=UFSet.next f in
	  (count.(6)<-count.(6)+1;
	   Some(Cut(7,toCut,accept,accept,fNone)))
      else let (toCut,a')=UASet.next a in
	(count.(5)<-count.(5)+1;
	 Some(Cut(7,UF.build (Lit toCut),accept,accept,fNone)))

    let rec findaction atms alternative =
      match !stack with
	| []       -> alternative
	| (f,n,_)::h when not (UASet.is_in (MyTheory.Atom.negation n) atms)->
	    (count.(2)<-count.(2)+1;
	     stack:=h;
	     fun()->Some(Focus(f,accept,fNone)))
	| (f,n,_)::h ->
	    stack:=h;
	    findaction atms alternative


    let model = function
      | Seq.EntF(atomN, g, formP, formPSaved, polar)      -> atomN
      | Seq.EntUF(atomN, delta, formP, formPSaved, polar) -> atomN

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
	  ->(match !address with
	       | Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp)
	       | Yes(exp) -> failwith("Yes not expected")
	       | _ -> address:=No);
	    if inloop then solve(machine(true,(count.(0),tset),(fun _ -> Exit Accept),fNone))
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
		let alternative() = Some(Search(Me.search4success,
						(function Local (Success _) -> count.(4)<-count.(4)+1;Accept | _-> Accept),
						F (cut_series (decide atms tset'))))
		in
		  (if !Flags.debug >0 then 
		     (let u,u' = UASet.cardinal tset,UASet.cardinal tset' in
			if u'>0 then print_endline(string_of_int u')
			else if u>0 then report()));
		  solve (machine (true,
				  (count.(0),tset'),
				  (fun _ -> Mem(Me.tomem,Accept)),
				  match action with  
				    | Some action -> (fun()->Some action)
				    | None        -> findaction atms alternative
				 )
			)
	      )
		
	(* When we are asked for focus: *)

	(* If there is no more positive formulae to place the focus on,
	   we restore the formulae on which we already placed focus *)

	| Fake(AskFocus(_,l,true,_,machine,_)) when UFSet.is_empty l
	    ->  solve (machine(Restore fNone))

	| Fake(AskFocus(_,l,false,_,machine,_)) when UFSet.is_empty l
	    -> solve (machine(Search(Me.search4failure,
				     (function Local (Fail _) -> count.(9)<-count.(9)+1;Accept|_->Accept),
				     A(fun()->Some(ConsistencyCheck(accept,fNone))))))

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
	    solve (machine(Search(Me.search4failure,
				  (function Local (Fail _) -> count.(9)<-count.(9)+1;Accept|_->Accept),
				  A(findaction atms (clause_pick atms l)))))


	(* When we are asked a side, we always go for the left first *)

	| Fake(AskSide(seq,machine,_)) -> solve (machine true)

	(* When kernel has explored all branches and proposes to go
	   backwards to close remaining branches, we give it the green
	   light *)

	| Fake(Stop(b1,b2, machine))   -> solve (machine ())

	(* When the kernel gives us a final answer, we return it and clear all the caches *)

	| Local ans                    -> report(); stack := []; is_init:=true; address:=No;
	    H.clear watched; Me.clear(); UF.clear(); UASet.clear(); UFSet.clear();MyTheory.Atom.clear();
	    for i=0 to Array.length count-1 do count.(i) <- 0 done;
	    ans

  end

end
