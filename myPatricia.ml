open Formulae
open Collection
open Strategy
open Sequents
open Patricia

open MyPatASet
open MySFormulaImplem
open MyDPLL

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
	above *)

     module UASet = MyPatA
     module UF    = MyDPLLForm
     module TMP   = struct 
       include UF
       module PF = PrintableFormula(UF)
       let toString = PF.toString
     end
     module UFSet = MyPatriciaCollectImplem(TMP)

    (* module UFSet = MyDPLLFSet *)

     let count = [|0;0;0|]
	   
     module Strategy =
       functor (FE:FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module Me = Memo(UFSet.Ext)(UASet.Ext)

	 let focus_pick h l =
	   let tofocus =
	     (*   if !Flags.unitp then
		  (if !Flags.debug>0&& ((count.(0)+count.(1)+count.(2)) mod Flags.every.(7) ==0)
		  then print_endline("Backtrack/Unit Propagate/Decide= "^string_of_int count.(0)^"/"^string_of_int count.(1)^"/"^string_of_int count.(2));
		  match UFSet.schoose h l with
		  | A a       -> count.(0)<-count.(0)+1;a
		  | F(Some a) -> count.(1)<-count.(1)+1;a
		  | _         -> count.(2)<-count.(2)+1;UFSet.choose l)
		  else *)
	     UFSet.choose l
	   in 
	     Focus(tofocus,accept,None)

	 let rec cut_series (a,f) =
	     if ASet.is_empty a then
	       if FSet.is_empty f then
	         None
	       else let (toCut,f')=FSet.next f in
		 Some(Cut(7,toCut,accept,accept,cut_series(a,f')))
	     else let (toCut,a')=ASet.next a in
	       Some(Cut(7,UF.build (Lit toCut),accept,accept,cut_series(a',f)))

	 let rec solve = function
	   | Local ans                    ->  Me.clear();ans

	   | Fake(AskFocus(_,l,machine,_)) when UFSet.is_empty l
	       -> solve (machine(Restore None))
	   | Fake(AskFocus(seq,l,machine,_)) when !Flags.memo
	       -> let (a,_)=Seq.simplify seq in
		 solve (machine(Search(Me.search4failure,accept,A(Some(focus_pick a l)))))
	   | Fake(AskFocus(seq,l,machine,_))
	     -> let (a,_)=Seq.simplify seq in
	       solve (machine (focus_pick a l))

	   | Fake(Notify(_,_,machine,_))     when !Flags.memo
	       -> solve (machine (true,
				  (),
				  (fun _ -> Mem(Me.tomem,Accept)),
				  Some(Search(Me.search4success,accept,if !Flags.almo then F(cut_series) else A(None)))
				 ))
	   | Fake(Notify(_,_,machine,_))  -> solve (machine (true,(),(fun _ -> Exit(Accept)),None))

	   | Fake(AskSide(seq,machine,_)) -> solve (machine true)
	   | Fake(Stop(b1,b2, machine))   -> solve (machine ())

       end

   end:User)
