open Lib
open Kernel

open Interfaces
open Sums

module GenPlugin(Atom: AtomType):(Plugins.Type with type literals = Atom.t) = struct
  
  type literals = Atom.t
  module DataStruct = DataStructures.Generate(Atom)
  module UASet = DataStruct.ASet
  module UF    = DataStruct.F
  module UFSet = DataStruct.FSet

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
    include Common.Utils.FEext(FE)
    module Me = Common.Utils.Memo(Atom)(FE)(UFSet)(UASet)

    type data       = int
    let initial_data= 0
    let address     = ref No

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

    let focus_pick seq l olda ()=count.(0)<-count.(0)+1; 
      if UFSet.is_empty l 
      then ConsistencyCheck(accept,fNone)
      else let (h,_)=Seq.simplify seq in
           if !Flags.unitp
           then (match UFSet.schoose h l with
	   | A a       ->if !Flags.debug>1 then print_endline("Yes "^Form.toString a);
	     address:=Yes(olda);
	     count.(1)<-count.(1)+1;
		    (* let now = count.(4) in *)
		    (* let myaccept = function  *)
		    (*   | Local(Success _) when count.(4)==now ->address:=No;Accept *)
		    (*   | _-> failwith "Expected Success" *)
		    (* in *)
	     Focus(a,accept,fNone)
	   | F(Some a) ->if !Flags.debug>1 then print_endline("Almost "^Form.toString a);
	     address:=Almost(olda);
	     count.(2)<-count.(2)+1;
	     Focus(a,accept,fNone)
	   | _         ->
	     address:=No;
	     count.(3)<-count.(3)+1;
	     match UFSet.rchoose h l with
	     | A a       -> if !Flags.debug>1 then print_endline("Random focus on "^Form.toString a);
	       Focus(a,accept,fNone)
	     | _         -> let a = UFSet.choose l in
			    if !Flags.debug>1 then print_endline("Random problematic focus on "^Form.toString a);
			    Focus(a,accept,fNone))
           else
	     Focus(UFSet.choose l,accept,fNone)

    let print_state olda =
      string_of_int count.(4)^" notifies, "^
	string_of_int count.(0)^" focus, with Backtrack / Unit propagate / Decide = "^
	string_of_int count.(1)^"/"^
	string_of_int count.(2)^"/"^
	string_of_int count.(3)
	
    let rec solve = function

      (* When the kernel gives us a final answer, we return it and clear all the caches *)

      | Local ans                    ->
	  Me.report();Me.clear(); print_endline("   Plugin's report:"); print_endline(print_state 0); print_endline "";
	  UF.clear(); UASet.clear(); UFSet.clear();Atom.clear();address:=No;
	  for i=0 to Array.length count-1 do count.(i) <- 0 done;
	  ans

      (* When we are asked for focus: *)

      (* If there is no more positive formulae to place the focus on,
	 we restore the formulae on which we already placed focus *)

      | Fake(AskFocus(_,l,true,_,machine,_)) when UFSet.is_empty l
	  -> solve (machine(Restore fNone))

      (* If Memoisation is off, we run focus_pick to determine which
	 action to perform, passing it the set of atoms, the set of
	 formulae on which focus is allowed, and the address of the
	 current focus point *)

      | Fake(AskFocus(seq,l,_,_,machine,olda)) when not !Flags.memo
	  -> solve (machine(focus_pick seq l olda ()))

      (* If Memoisation is on, we
	 - start searching whether a bigger sequent doesn't already
	 have a counter-model
	 - accept the result of that search
	 - in case we have not found happiness in memoisation table,
	 run focus_pick to determine action to perform, passing it
	 the set of atoms, the set of formulae on which focus is
	 allowed, and the address of the current focus point
      *)

      | Fake(AskFocus(seq,l,_,_,machine,olda))
	  -> solve(machine(Me.search4failureNact seq (focus_pick seq l olda)))

      (* When we are notified of new focus point: *)


      (* If Memoisation is off, we
	 - accept defeat if kernel has detected a loop and proposes to fail
	 - pass to the kernel address 0 for new focus point
	 - our exit_function just accepts every result
	 - no further instruction is given to kernel
      *)

      | Fake(Notify(_,_,machine,olda))  when not !Flags.memo
          ->if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
            then print_endline(print_state olda);
            count.(4)<-count.(4)+1;
            solve (machine (true,0,accept,fNone))

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

      | Fake(Notify(seq,_,machine,olda))     
	-> (* (match !address with *)
	    (* 	| Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp) *)
	    (* 	| Yes(exp) -> failwith("Yes not expected") *)
	    (* 	| _ -> address:=No); *)
	    (* if !Flags.debug>0&& count.(0) ==100000 then failwith("stop") else*)
	if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	then print_endline(print_state olda);
	  count.(4)<-count.(4)+1;
	  solve(machine(true,count.(4),Me.memAccept,Me.search4successNact seq fNone))


      (* When we are asked a side, we always go for the left first *)

      | Fake(AskSide(seq,machine,_)) -> solve (machine true)

      (* When kernel has explored all branches and proposes to go
	 backwards to close remaining branches, we give it the green
	 light *)

      | Fake(Stop(b1,b2, machine))   -> solve (machine ())

  end

end
