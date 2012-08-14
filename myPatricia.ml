open Patricia
open MyPatASet
open MyDPLL

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
	above *)

     module UASet = MyPatA
     module UF    = MySFormulaImplem


     module TMP   = struct 
       include UF
       module PF = Formulae.PrintableFormula(UF)
       let toString = PF.toString
     end
     module UFSet = MyPatriciaCollectImplem(TMP)
     
   (*        module UFSet = MyDPLLFSet *)

     let count = [|0;0;0;0;0|]
       
     module Strategy =
       functor (FE:Sequents.FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) -> struct
	 include FE

	 type data       = int
	 let initial_data= 0
	 let address     = ref No

	 module Me = Memo(UFSet.Ext)(UASet.Ext)
	 module PF = Formulae.PrintableFormula(UF)

	 let focus_pick h l olda= count.(0)<-count.(0)+1; Focus(UFSet.choose l,accept,None)

(*	 let focus_pick h l olda= 
	   if !Flags.unitp
	     then (count.(0)<-count.(0)+1;
	     match UFSet.schoose h l with
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
			 if !Flags.debug>0 then print_endline("Random problematic focus on "^PF.toString a);
			 Focus(a,accept,None))
	   else
	     focus_pick h l olda
*)

	 let print_state olda = (* if !Flags.debug>0&& count.(0) ==100000 then failwith("stop") else*)
	   if !Flags.debug>0&& (count.(0) mod Flags.every.(7) ==0)
	   then print_endline("Notify = "^
				string_of_int count.(4)^
				" with old address = "^
				string_of_int olda^
				" and Focus = "^
				string_of_int count.(0)^
				"(Backtrack/Unit Propagate/Decide) "^
				string_of_int count.(1)^"/"^
				string_of_int count.(2)^"/"^
				string_of_int count.(3))

	 let rec cut_series (a,f) =
	     if ASet.is_empty a then
	       if FSet.is_empty f then
	         None
	       else let (toCut,f')=FSet.next f in
		 Some(Cut(7,toCut,accept,accept,cut_series(a,f')))
	     else let (toCut,a')=ASet.next a in
	       Some(Cut(7,UF.build (Formulae.Lit toCut),accept,accept,cut_series(a',f)))

	 let rec solve = function
	   | Local ans                    -> Me.clear();UF.clear(); UASet.clear(); UFSet.clear();Formulae.Atom.clear();
	       for i=0 to Array.length count-1 do count.(i) <- 0 done;
	       address:=No;
	       ans

	   | Fake(AskFocus(_,l,machine,_)) when UFSet.is_empty l
	       -> solve (machine(Restore None))
	   | Fake(AskFocus(seq,l,machine,olda)) when !Flags.memo
	       -> let (a,_)=Seq.simplify seq in
		 solve (machine(Search(Me.search4failure,accept,A(Some(focus_pick a l olda)))))
	   | Fake(AskFocus(seq,l,machine,olda))
	     -> let (a,_)=Seq.simplify seq in
	       solve (machine (focus_pick a l olda))

	   | Fake(Notify(_,_,machine,olda))     when !Flags.memo
	       -> (match !address with
		     | Almost(exp) when exp<>olda -> failwith("Expected another address: got "^string_of_int olda^" instead of "^string_of_int exp)
		     | Yes(exp) -> failwith("Yes not expected")
		     | _ -> address:=No);
		 print_state olda;
		 count.(4)<-count.(4)+1;
		 solve (machine (true,
				 count.(4),
				 (fun _ -> Mem(Me.tomem,Accept)),
				 Some(Search(Me.search4success,accept,if !Flags.almo then F(cut_series) else A(None)))
				))
	   | Fake(Notify(_,_,machine,_))  -> solve (machine (true,0,(fun _ -> Exit(Accept)),None))

	   | Fake(AskSide(seq,machine,_)) -> solve (machine true)
	   | Fake(Stop(b1,b2, machine))   -> solve (machine ())

       end

   end:Strategy.User)
