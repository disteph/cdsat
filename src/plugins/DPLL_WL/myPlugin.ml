open Lib
open Kernel
open Formulae
open Patricia
open Sums
open MyPatASet
open MyDPLLStructures

module UASet = MyPatA
module UF    = MyDPLLForm
module UFSet = MyDPLLFSet

let count = [|0;0;0;0;0;0;0;0;0;0|]
  
module Strategy (FE:Sequents.FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) = struct
  include FE

  type data       = int
  let initial_data= 0
  let address     = ref No

  module Me = Memo(UFSet.Ext)(UASet.Ext)
  module PF = Formulae.PrintableFormula(UF)

  (* We record a stack of clauses that will definitely do a Unit Propagate *)
  let stack   = ref []


  let print_state olda =
    string_of_int count.(0)^" notifies, 
Backtrack / Unit propagate / Decide / Success / Unit Propagate / Weird = "^
      string_of_int count.(1)^"/"^
      string_of_int count.(2)^"/"^
      string_of_int count.(3)^"/"^
      string_of_int count.(4)^"/"^
      string_of_int count.(5)^"/"^
      string_of_int count.(6)

  (* The following structures implement the table of watched literals *)

  module CSetWatched = struct
    type keys        = MyDPLLFSet.UT.keys
    let kcompare     = MyDPLLFSet.UT.compare
    type values      = Atom.t
    let vcompare     = Atom.compare
    type infos       = unit
    let info_build   = empty_info_build
    let treeHCons    = true
  end

  module CSet = PATMap(CSetWatched)(MyDPLLFSet.UT)

  module H = Hashtbl.Make(Atom)
  let watched = H.create 5003

  let tablecheck () =
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

  let initialise atms = 
    UFSet.iter 
      (fun formula -> match picklit formula atms 2 with
	 | lit1::lit2::_ -> (addlit lit1 formula lit2 ;
			     addlit lit2 formula lit1 )
	 | _ -> ()
      )

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
	  (match UFSet.sub (MyDPLLFSet.UT.tag j) (Some(atms),-1) None with
	     | Yes _    ->
		 (* Clause j is allowing backtrack;
		    we stop investigating the table of watched literals and return j *)
		 (Some j,t)
	     | Almost n when UFSet.filter atms n ->
		 (* Clause j is allowing Unit Propagation;
		    we stack it, and leave the table of watched literals unchanged *)
		 stack:= (j,false)::!stack;
		 (None,t)
	     | Almost n -> (None,t)
	     | _ ->
		 (* Clause j allows neither backtrack nor unit propagate;
		    we need to change the watched literal lit *)
		 (match UF.aset j with
		    | None   -> failwith("Should find a set of atoms")
		    | Some l ->
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

  let update atms olda = match UASet.latest atms with
    | Some lit when H.mem watched lit ->
	let l = H.find watched lit in
	(*print_endline(Atom.toString lit^" "^CSet.toString None (fun (j,x)->PF.toString j^","^Atom.toString x) l);
	print_endline("");*)
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
		   Some(Focus(a,myaccept,None))
	     | None -> None
	  )
    | _ -> None


  let rec cut_series (a,f) =
    if ASet.is_empty a then
      if FSet.is_empty f then
	None
      else let (toCut,f')=FSet.next f in
	(count.(6)<-count.(6)+1;
	 Some(Cut(7,toCut,accept,accept,cut_series(a,f'))))
    else let (toCut,a')=ASet.next a in
      (count.(5)<-count.(5)+1;
       Some(Cut(7,UF.build (Formulae.Lit toCut),accept,accept,cut_series(a',f))))

  let findaction alternative =
    match !stack with
      | []       -> (*print_endline("Empty stack");*)alternative()
      | (f,_)::h -> (*print_endline(PF.toString f);*)
	  count.(2)<-count.(2)+1;
	  stack:=h;
	  Focus(f,(*(function Local(Fail _)->print_endline(PF.toString f^" failed");Accept|_->print_endline(PF.toString f^" succeeded");Accept)*)
		accept,None)

  (* focus_pick chooses a focus
     h is the set of literals
     l is the set of (negated) clauses
     schoose looks at whether there is a (negated) clause in l such
     that every literal in it is in h
     - answers A(a) if a is such a clause
     - answers F(Some a) if a is almost such a clause (were it not
     for one literal)
     - answers F(None) if there is no clause satisfying the above
  *)

  let focus_pick h l =
    if not(!stack=[]) then failwith("pas []");
    count.(3)<-count.(3)+1;
    match UFSet.rchoose h l with
      | A a       -> if !Flags.debug>1 then print_endline("Random focus on "^PF.toString a);
	  Focus(a,accept,None)
      | _         -> let a = UFSet.choose l in
	  if !Flags.debug>1 then print_endline("Random problematic focus on "^PF.toString a);
	  Focus(a,accept,None)

  let model = function
    | Seq.EntF(atomN, g, formP, formPSaved, polar)      -> atomN
    | Seq.EntUF(atomN, delta, formP, formPSaved, polar) -> atomN

  let rec solve = function

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

    | Fake(Notify(seq,inloop,machine,olda))
      -> (match !address with
	    | Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp)
	    | Yes(exp) -> failwith("Yes not expected")
	    | _ -> address:=No);
	if inloop then solve(machine(true,count.(0),(fun _ -> Exit Accept),None))
	else
	  (if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	   then print_endline(print_state olda);
	   count.(0)<-count.(0)+1;
	   (if !is_init then
	      let (atms,cset)= Seq.simplify seq in 
		initialise atms cset;is_init:=false);
	   let alternative() = Search(Me.search4success,
				      (function Local (Success _) -> count.(4)<-count.(4)+1;Accept | _-> Accept),
				      F cut_series)
	   in
	   let next_action = 
	     match update (model seq) olda with  
	       | Some action -> action
	       | None        -> findaction alternative
	   in 
	     solve (machine (true,
			     count.(0),
			     (fun _ -> Mem(Me.tomem,Accept)),
			     Some next_action
			    )
		   )
	  )
    (* When we are asked for focus: *)

    (* If there is no more positive formulae to place the focus on,
       we restore the formulae on which we already placed focus *)

    | Fake(AskFocus(_,l,machine,_)) when UFSet.is_empty l
	-> solve (machine(Restore None))

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

    | Fake(AskFocus(seq,l,machine,olda))
	-> let alternative() = focus_pick (model seq) l in
	solve (machine(Search(Me.search4failure,
			      (function Local (Fail _) -> count.(9)<-count.(9)+1;Accept|_->Accept),
			      A(Some(findaction alternative)))))


    (* When we are asked a side, we always go for the left first *)

    | Fake(AskSide(seq,machine,_)) -> solve (machine true)

    (* When kernel has explored all branches and proposes to go
       backwards to close remaining branches, we give it the green
       light *)

    | Fake(Stop(b1,b2, machine))   -> solve (machine ())

    (* When the kernel gives us a final answer, we return it and clear all the caches *)

    | Local ans                    -> H.clear watched; stack := [];is_init:=true;
	Me.clear(); print_endline("   Plugin's report:"); print_endline(print_state 0); print_endline "";
	UF.clear(); UASet.clear(); UFSet.clear();Formulae.Atom.clear();address:=No;
	for i=0 to Array.length count-1 do count.(i) <- 0 done;
	ans

end
