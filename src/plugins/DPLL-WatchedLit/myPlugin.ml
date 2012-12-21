open Lib
open Kernel
open Formulae
open Patricia
open Sums
open MyPatASet_W
open MyDPLLStructures_W

module MyPlugin : Plugin.Type = struct

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

    module H = Hashtbl.Make(Atom)
    let table = H.create 5003
    let unique = ref 0
    let free = ref UASet.empty
    let sequent = ref UASet.empty
    let stack = ref []
    let etape = ref 1
    let prioritaire = ref None

    let init()=
      (H.clear table;
      unique := 0;
      free := UASet.empty;
      sequent := UASet.empty;
      stack := [];
      etape := 1;
      prioritaire := None)
      

    let analyse_litteral l c clause=		
      if not (UASet.is_in l !free) then (free := UASet.add l !free;free:= UASet.add (Atom.negation l) !free);
      (try let _ = H.find table (Atom.negation l) in () with Not_found -> H.add table (Atom.negation l) (0,UFSet.empty));
      if c<2
      then try H.replace table l (0,let (_,x) = H.find table l in UFSet.add clause x);c+1 
      with Not_found -> H.add table l (0,UFSet.add clause UFSet.empty);
	c+1
      else try (let _= H.find table l in ());c
      with Not_found -> H.add table l (0,UFSet.empty);
	c 

    let rec analyse_clause x c clause = 
      match UF.reveal x with
	|AndP(x1,x2)  -> let i = analyse_clause x1 c clause in analyse_clause x2 i clause
	|Lit l -> analyse_litteral l c clause 
	| _ -> failwith("Can't handle formula")

    let rec analyse x = 
      if not (UFSet.is_empty x) then let (a,l) = UFSet.next x in analyse_clause a 0 a;analyse l
	
    let cut () = UF.build(let (x,_) = UASet.next !free in Lit(x))
      



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


(*    let focus_pick h l olda=count.(0)<-count.(0)+1; 
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
*)

    let print_state olda =
      string_of_int count.(4)^" notifies, "^
	string_of_int count.(0)^" focus, with Backtrack / Unit propagate / Decide = "^
	string_of_int count.(1)^"/"^
	string_of_int count.(2)^"/"^
	string_of_int count.(3)
	
    let rec cut_series (a,f) =
      if ASet.is_empty a then
	if FSet.is_empty f then
	  None
	else let (toCut,f')=FSet.next f in
	  Some(Cut(7,toCut,accept,accept,cut_series(a,f')))
      else let (toCut,a')=ASet.next a in
	Some(Cut(7,UF.build (Formulae.Lit toCut),accept,accept,cut_series(a',f)))



    let backtrack s1 s2 =
      let rec back_aux s3 =
	if not(UASet.is_empty s3) then
	  let (x1,y1) = UASet.next s3 in
	    free:= UASet.add x1 !free;
	    free:= UASet.add (Atom.negation x1) !free;
	    if not (H.mem table x1) then H.add table x1 (0,UFSet.empty);
	    H.replace table x1 (0,let (_,x) = H.find table x1 in x); 
	    (let y = Atom.negation x1 in 
	       if not (H.mem table y) then H.add table y (0,UFSet.empty);
	       H.replace table y (0,let (_,x) = H.find table y in x));
	    stack  := [];
	    sequent:= UASet.remove x1 !sequent;
	    back_aux y1
      in
	back_aux(UASet.diff s1 s2)

    let rec anal_clause x2 z claus free= match UF.reveal z with
      | AndP(u,v) -> let (boo,fr) = anal_clause x2 u claus free in if boo then anal_clause x2 v claus fr else (false,0)
      | Lit l -> let (i,_) = H.find table l in
	  begin
	    match i with
	    | 2 -> (false,0)
	    | 1 -> (true,free)
	    | 0 -> if free == 1 then begin 
		H.replace table l (0,let(_,m) = H.find table l in UFSet.add claus m);
		H.replace table x2 (1, let(_,m) = H.find table x2 in UFSet.remove claus m);
		(false,0)
	      end
	      else (true,1)
	  end
      | _ -> failwith("Can't handle formula") 

    let rec clause x2 y =
      if not(UFSet.is_empty y) then
	let (a,b)=UFSet.next y in
	  match anal_clause x2 a a 0 with
	  |true,0 -> prioritaire := Some(a);
	  |true,_ -> stack:= a::!stack; clause x2 b
	  |_,_->clause x2 b											


    let backtrack_sym s1 s2 =
      let rec back_aux s3 =
	if not(UASet.is_empty s3) then
	  let (x2,y2) = (UASet.next s3) in
	    if UASet.is_in x2 !free then free := UASet.remove x2 !free;
	    if UASet.is_in (Atom.negation x2) !free then free := UASet.remove (Atom.negation x2) !free;
	    if not (H.mem table x2) then H.add table x2 (1,UFSet.empty);
	    H.replace table x2 (1,let (_,x) = H.find table x2 in x);
	    (let y = Atom.negation x2 in 
	       if not (H.mem table y) then H.add table y (2,UFSet.empty);
	       H.replace table y (2,let (_,x) = H.find table y in x));
	    (let (_,x) = H.find table x2 in match !prioritaire with None -> clause x2 x | _ -> ());
	    back_aux y2
      in
	back_aux(UASet.diff s2 s1)



    let rec solve = function

      (* When the kernel gives us a final answer, we return it and clear all the caches *)

      | Local ans                    -> init();
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
	  ->
	  if !unique=0 then (unique := !unique + 1; analyse l);
	  prioritaire:=None;
	  let (newsequent,_) = Seq.simplify seq in
	  backtrack     !sequent newsequent;
	  backtrack_sym !sequent newsequent;	  
	  sequent := newsequent;
	  (match seq with
	       Seq.EntF(_,_,_,_,_)->()
	     | Seq.EntUF(_,_,l',l'',_)->
		 (match !prioritaire with
		    | None ->
			begin
			  let rec analyse_stack s =
			    match !s with
			      | []   -> ()
			      | x::y ->
				  begin
				    s := y;
				    if UFSet.is_in x l
				    then prioritaire:= Some(x)
				    else analyse_stack s
				  end
			  in analyse_stack stack;
			end
		    |Some x -> ()
		 )
(*		    |Some x -> if not((UFSet.is_in x l')||(UFSet.is_in x l'')) then failwith("Pas autorisé "^PF.toString(x)) *)
	  );
	  
	  (match !prioritaire with
	    | Some(u) -> solve(machine(Search(Me.search4failure,accept,A(Some(Focus(u,accept,None)))))) 
	    | None -> if UASet.is_empty !free
	      then let (a,b) = UFSet.next l in solve(machine(Search(Me.search4failure,accept,A(Some(Focus(a,accept,None))))))
	      else solve(machine(Search(Me.search4failure,accept,A(Some(Cut(7,cut(),accept,accept,None))))))
	  )

      (* If Memoisation is off, we run focus_pick to determine which
           action to perform, passing it the set of atoms, the set of
           formulae on which focus is allowed, and the address of the
           current focus point *)

      | Fake(AskFocus(seq,l,machine,olda))
	-> failwith("didn't implement")

	  (*let (a,_)=Seq.simplify seq in
	  solve (machine (focus_pick a l olda))*)

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

      | Fake(Notify(_,_,machine,olda))     when !Flags.memo
	  -> (match !address with
		| Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp)
		| Yes(exp) -> failwith("Yes not expected")
		| _ -> address:=No);
	    (* if !Flags.debug>0&& count.(0) ==100000 then failwith("stop") else*)
	    if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	    then print_endline(print_state olda);
	    count.(4)<-count.(4)+1;
	    solve (machine (true,
			    count.(4),
			    (fun _ -> Mem(Me.tomem,Accept)),
			    Some(Search(Me.search4success,accept,if !Flags.almo then F(cut_series) else A(None)))
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

end
