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
     module UFSet = MyPatriciaCollectImplem(struct 
					      include UF
					      let id = UF.id
					      module PF = PrintableFormula(UF)
					      let toString = PF.toString
					    end)
     (*   module UFSet = MyDPLLFSet  *)
     (*     let focus_pick (h:UASet.CI.t) l   = match UFSet.choose h l with
	    | A a       -> a
	    | F(Some a) -> a
	    | _         -> UFSet.SS.choose l
     *)

     module Strategy =
       functor (FE:FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module Me = Memo(UFSet.Ext)(UASet.Ext)

	 let focus_pick l = Focus(UFSet.SS.choose l,accept,None)

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
		 solve (machine(Search(Me.search4failure,accept,A(Some(focus_pick l)))))
	   | Fake(AskFocus(_,l,machine,_)) -> solve (machine (focus_pick l))

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
