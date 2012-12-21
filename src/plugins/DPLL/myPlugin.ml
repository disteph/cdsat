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

let count = [|0;0;0;0;0|]
  
module Strategy (FE:Sequents.FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) = struct
  include FE

  type data       = int
  let initial_data= 0
  let address     = ref No

  module Me = Memo(UFSet.Ext)(UASet.Ext)
  module PF = Formulae.PrintableFormula(UF)

  (* focus_pick chooses a focus
     h is the set of literals
     l is the set of (negated) clauses
     schoose looks at whether there is a (begated) clause in l such
     that every literal in it is in h
     - answers A(a) if a is such a clause
     - answers F(Some a) if a is almost such a clause (were it not
     for one literal)
     - answers F(None) if there is no clause satisfying the above
  *)

  let focus_pick h l olda=count.(0)<-count.(0)+1; 
    if !Flags.unitp
    then (match UFSet.schoose h l with
	    | A a       ->if !Flags.debug>1 then print_endline("Yes "^PF.toString a);
		address:=Yes(olda);
		count.(1)<-count.(1)+1;
		let now = count.(4) in
		let myaccept = function 
		  | Local(Success _) when count.(4)==now ->address:=No;Accept
		  | _-> failwith "Expected Success"
		in
		  Focus(a,myaccept,None)
	    | F(Some a) ->if !Flags.debug>1 then print_endline("Almost "^PF.toString a);
		address:=Almost(olda);
		count.(2)<-count.(2)+1;
		Focus(a,accept,None)
	    | _         ->
		address:=No;
		count.(3)<-count.(3)+1;
		match UFSet.rchoose h l with
		  | A a       -> if !Flags.debug>1 then print_endline("Random focus on "^PF.toString a);
		      Focus(a,accept,None)
		  | _         -> let a = UFSet.choose l in
		      if !Flags.debug>1 then print_endline("Random problematic focus on "^PF.toString a);
		      Focus(a,accept,None))
    else
      Focus(UFSet.choose l,accept,None)

  let print_state olda =
    string_of_int count.(4)^" notifies, "^
      string_of_int count.(0)^" focus, with Backtrack / Unit propagate / Decide = "^
      string_of_int count.(1)^"/"^
      string_of_int count.(2)^"/"^
      string_of_int count.(3)
      



  module H = Hashtbl.Make(Atom)
  let watched = H.create 5003
  let stack   = ref []


  (* TODO:
     - initialise watched lit
     - search for presence of negative (eliminate clauses that are true)
     - replace UASet.next below
     - improve smartchoose
     - integrate watched lit for learnt clauses
  *)

  let treat_UP j = stack:= (j,false)::!stack

  let update atms olda = match UASet.latest atms with
    | Some lit when H.mem watched lit ->
	let l = H.find watched lit in
	let treat_N clause =
	  match UF.aset clause with
	    | None   -> ()
	    | Some s -> 
		let (newlit,_)= UASet.next s in
		  if not (H.mem watched newlit) then H.add watched newlit UFSet.empty; 
		  let l' = H.find watched newlit in
		    H.replace watched newlit (UFSet.add clause l')
	in
	let (answer,l') = UFSet.smartchoose atms l treat_UP treat_N in
	  (H.replace watched lit l'; 
	   match answer with
	     | Some a       -> if !Flags.debug>1 then print_endline("Yes "^PF.toString a);
		 address:=Yes(olda);
		 count.(1)<-count.(1)+1;
		 let now = count.(4) in
		 let myaccept = function 
		   | Local(Success _) when count.(4)==now ->address:=No;Accept
		   | _-> failwith "Expected Success"
		 in
		   Some(Focus(a,myaccept,None))
	     | None -> None
	  )
    | _ -> None
	
  let rec cut_series (a,f) =
    if ASet.is_empty a then
      if FSet.is_empty f then
	None
      else let (toCut,f')=FSet.next f in
	Some(Cut(7,toCut,accept,accept,cut_series(a,f')))
    else let (toCut,a')=ASet.next a in
      Some(Cut(7,UF.build (Formulae.Lit toCut),accept,accept,cut_series(a',f)))

  let rec solve = function

    (* When the kernel gives us a final answer, we return it and clear all the caches *)

    | Local ans                    -> H.clear watched; stack := [];
	Me.clear(); print_endline("   Plugin's report:"); print_endline(print_state 0); print_endline "";
	UF.clear(); UASet.clear(); UFSet.clear();Formulae.Atom.clear();address:=No;
	for i=0 to Array.length count-1 do count.(i) <- 0 done;
	ans

    (* When we are asked for focus: *)

    (* If there is no more positive formulae to place the focus on,
       we restore the formulae on which we already placed focus *)

    | Fake(AskFocus(_,l,machine,_)) when UFSet.is_empty l
	-> solve (machine(Restore None))

    (* If Memoisation is on, we
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

    | Fake(AskFocus(seq,l,machine,olda)) when !Flags.memo
	-> let (a,_)=Seq.simplify seq in
	  solve (machine(Search(Me.search4failure,accept,A(Some(focus_pick a l olda)))))

    (* If Memoisation is off, we run focus_pick to determine which
       action to perform, passing it the set of atoms, the set of
       formulae on which focus is allowed, and the address of the
       current focus point *)

    | Fake(AskFocus(seq,l,machine,olda))
      -> let (a,_)=Seq.simplify seq in
	solve (machine (focus_pick a l olda))

    (* When we are notified of new focus point: *)

    (* If Memoisation is on, we
       - accept defeat if kernel has detected a loop and proposes to fail
       - pass to the kernel an address for new focus point (count.(4)+1)
       - our exit_function is always instructing kernel to memoise
       result and accept it
       - instruct the kernel to look for success in memoisation table
       - accept the result of that search
       - if almo flag is on, the search should be approximate, and
       cut_series says what to do with approximate results
       - if almo flag is off, the search is exact and no further
       instruction is given to kernel
    *)

    | Fake(Notify(seq,_,machine,olda))     when !Flags.memo
	-> (match !address with
	      | Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp)
	      | Yes(exp) -> failwith("Yes not expected")
	      | _ -> address:=No);
	  (* if !Flags.debug>0&& count.(0) ==100000 then failwith("stop") else*)
	  if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	  then print_endline(print_state olda);
	  count.(4)<-count.(4)+1;
	  let (atms,_)= Seq.simplify seq in
	    solve (machine (true,
			    count.(4),
			    (fun _ -> Mem(Me.tomem,Accept)),
			    Some(Search(Me.search4success,accept,if !Flags.almo then 
					  F(fun z-> update atms olda) else A(None)))
			   ))

    (* If Memoisation is off, we
       - accept defeat if kernel has detected a loop and proposes to fail
       - pass to the kernel address 0 for new focus point
       - our exit_function just accepts every result
       - no further instruction is given to kernel
    *)

    | Fake(Notify(_,_,machine,olda))  ->
	if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	then print_endline(print_state olda);
	count.(4)<-count.(4)+1;
	solve (machine (true,0,(fun _ -> Exit(Accept)),None))

    (* When we are asked a side, we always go for the left first *)

    | Fake(AskSide(seq,machine,_)) -> solve (machine true)

    (* When kernel has explored all branches and proposes to go
       backwards to close remaining branches, we give it the green
       light *)

    | Fake(Stop(b1,b2, machine))   -> solve (machine ())

end
