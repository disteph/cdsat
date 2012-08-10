open Formulae
open MySFormulaImplem
open Collection
open Strategy
open Sequents
open MyPatASet
open Patricia

module MyPAT =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
	above *)

     module UF    = MySmartFormulaImplem
     module UFSet = MyPatriciaCollectImplem(struct 
					      include UF
					      let id = UF.id
					      module PF = PrintableFormula(UF)
					      let toString = PF.toString
					    end)
     module UASet = MyPatriciaACollectImplem

     module Strategy =
       functor (FE:FrontEndType with module F=UF.FI and module FSet=UFSet.CI and module ASet=UASet.CI) -> struct
	 include FE

	 type data = unit
	 let initial_data=()

	 module ASetExt = struct
	   include UASet.CI
	   let compare  = UASet.SS.compare
	   let compareE t1 t2  =
	     let (b1,pred1,tl1) = Atom.reveal t1 in 
	     let (b2,pred2,tl2) = Atom.reveal t2 in 
	     let c = Pervasives.compare (UASet.SS.tag (b1,pred1))(UASet.SS.tag (b2,pred2)) in
	       if c=0 then Pervasives.compare (Atom.id t1) (Atom.id t2) else c
	   let min s      = match UASet.SS.info s with
	     | None       -> None
	     | Some(k,v)  -> UASet.AtSet.SS.info v 
	   let diff       = UASet.SS.diff (fun k x y -> UASet.lleaf(k,UASet.AtSet.SS.diff x y))
	   let first_diff s1 s2 = match UASet.SS.first_diff UASet.SS.info s1 s2 with
	     | (None,b)      -> (None,b)
	     | (Some(k,x),b) -> let other = if b then s2 else s1 in
		 if UASet.SS.mem k other
		 then  UASet.AtSet.SS.first_diff (UASet.AtSet.SS.info) x (UASet.SS.find k other)
		 else (UASet.AtSet.SS.info x,b)
	 end

	 module FSetExt = struct
	   include UFSet.CI
	   let compare    = UFSet.SS.compare
	   let compareE   = UF.compare
	   let min        = UFSet.SS.info
	   let diff       = UFSet.SS.diff
	   let first_diff = UFSet.SS.first_diff min
	 end

	 module Me = Memo(FSetExt)(ASetExt)


	 let focus_pick l   = Focus(UFSet.SS.choose l, accept,None)

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
	   | Fake(AskFocus(_,l,machine,_)) when !Flags.memo
	       -> solve (machine(Search(Me.search4failure,accept,A(Some(focus_pick l)))))
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
