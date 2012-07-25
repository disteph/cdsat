open Formulae;;
open Collection;;
open Sequents;;

let debug = ref 0;;          (* Activates debug mode (displays fails, etc) *)
let loop_detect = ref true;; (* Activates loop detection *)

module ProofSearch =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(struct
	   
	   include FrontEnd(F)(FSet)(ASet)
	   include Ans
	     
	   let count = [|0;0;0;0;0|]
	   let dir = ref true

	   let throw ans s = 
	     let (index, word, modulo) =
	       match ans with
		 | Local(Success(_)) -> (0,"LocalSuccess",100000)
		 | Local(Fail)       -> (1,"LocalFail",100000) 
		 | Fake(a,b,_)   -> let mydir = if b then "right" else "left" in
		     if a then (2,"FakeSuccess->"^mydir,1) else (3,"FakeFail->"^mydir,1)
	     in
	       count.(index) <- count.(index) + 1;
	       if !debug>0  then 
		 if (((count.(index)) mod modulo) ==0) then
		   begin
		     print_int(count.(index));
		     if !debug==1 then print_endline(" "^word^": ");
		     if !debug>1  then print_endline(" "^word^": "^Seq.toString s);
		   end; 
	       ans

	   let rec ou v1 v2 fseq1 fseq2 cont =
	     let newcont1 = function
	       | Local(Success prooftree1)       -> cont (Local(Success(fseq1 prooftree1)))
	       | Local Fail ->
		   let newcont2 = function
		     | Local(Success(prooftree2))-> cont (Local(Success(fseq2 prooftree2)))
		     | Local Fail                -> cont (Local Fail)
		     | Fake(b1,b2,Comp f2)       -> cont (Fake(b1,b2,Comp(ou (fun cc -> cc (Local Fail)) f2 fseq1 fseq2)))
		   in
		     v2 newcont2
	       | Fake(false,true,Comp f1) ->
		   let newcont2 = function
		     | Local(Success prooftree2) -> cont (Local(Success(fseq2 prooftree2)))
		     | Local Fail                -> cont (Fake(false,true,Comp(ou f1 (fun cc -> cc (Local(Fail))) fseq1 fseq2)))
		     | Fake(false,false,Comp f2) -> ou f1 f2 fseq1 fseq2 cont
		     | Fake(b1,b2,Comp f2)       -> cont (Fake(b1,b2,Comp(ou (fun cc -> cc (Fake(false,true,Comp f1))) f2 fseq1 fseq2)))
		   in
		     v2 newcont2
	       | Fake(b1,b2,Comp f1)             -> cont (Fake(b1,b2,Comp(ou f1 v2 fseq1 fseq2)))
	     in
	       v1 newcont1

	   let rec et v1 v2 fseq cont =
	     let newcont1 = function
	       | Local(Success prooftree1) ->
		   let newcont2 = function
		     | Local(Success prooftree2)-> cont (Local(Success(fseq prooftree1 prooftree2)))
		     | Local Fail               -> cont (Local Fail)
		     | Fake(b1,b2,Comp f2)      -> cont (Fake(b1,b2,Comp(et (fun cc -> cc (Local(Success prooftree1))) f2 fseq)))
		   in
		     v2 newcont2
	       | Local Fail                     -> cont (Local Fail)
	       | Fake(true,true,Comp f1) ->
		   let newcont2 = function
		     | Local(Success prooftree2)-> cont (Fake(true,true,Comp(et f1 (fun cc -> cc (Local(Success prooftree2))) fseq)))
		     | Local Fail               -> cont (Local Fail)
		     | Fake(true,false,Comp f2) -> et f1 f2 fseq cont
		     | Fake(b1,b2,Comp f2)      -> cont (Fake(b1,b2,Comp(et (fun cc -> cc (Fake(true,true,Comp f1))) f2 fseq)))
		   in
		     v2 newcont2
	       | Fake(b1,b2,Comp f1)            -> cont (Fake(b1,b2,Comp(et f1 v2 fseq)))
	     in
	       v1 newcont1

       let rec straight v fseq cont =
	 let newcont = function
	   | Local(Success prooftree)-> cont (Local(Success(fseq prooftree)))
	   | Local Fail              -> cont (Local Fail)
	   | Fake(b1,b2,Comp f)      -> cont (Fake(b1,b2,Comp(straight f fseq)))
	 in
	   v newcont

       (*
	* Main Search function 
	* delta = Formulae to be asynchronously decomposed 
	* gammatomN = negative atoms found in asynchronous phase (negation symbol not stored)
	* gammaformP = positive formulae found in asynchronous phase (focus to be placed on them later)
	* gammaformPTried = positive formulae found in asynchronous phase (focus on them has failed)
	* gammaformPSaved = positive formulae on which focus has been placed "more times" than remaining formulae in gammaformP 
	* (These other formulae have priority for focus -> ensures fairness)
	* Returns Success(Prooftree) if a proof is found
	*)

       let rec lk_solve inloop seq cont : output =
	 (*	 print_endline("attack"^Seq.toString seq); *)
	 match seq with
	   | Seq.EntF(gammatomN, g, gammaformP, gammaformPSaved, polar)
             -> begin
	       match (F.reveal g) with
		 | _ when ((polarity polar g) <> Pos) ->
		     straight 
		       (lk_solve inloop (Seq.EntUF (gammatomN, FSet.add g FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)))
		       (fun pt->PT.build(PT.OnePre (seq, pt)))
		       cont
		       
		 | AndP(a1, a2) ->
		     let u1 = lk_solve inloop (Seq.EntF (gammatomN, a1, gammaformP, gammaformPSaved, polar)) in
		     let u2 = lk_solve inloop (Seq.EntF (gammatomN, a2, gammaformP, gammaformPSaved, polar)) in
		       et u1 u2 (fun pt1 pt2 -> PT.build(PT.TwoPre (seq, pt1,pt2))) cont
			 
		 | OrP(a1, a2) -> 
		     let side_pick b = 
		       let (a1',a2') = if b then (a1,a2) else (a2,a1) in
		       let u1 = lk_solve inloop (Seq.EntF (gammatomN, a1', gammaformP, gammaformPSaved, polar)) in
		       let u2 = lk_solve inloop (Seq.EntF (gammatomN, a2', gammaformP, gammaformPSaved, polar)) in
		       let fseq = fun pt -> PT.build(PT.OnePre(seq,pt)) in
			 ou u1 u2 fseq fseq cont
		     in
		       Fake(AskSide(seq,side_pick))

		 | Lit(b,f, tl) ->
		     if (ASet.is_in (b,f, tl) gammatomN) 
		     then cont (throw (Local(Success(PT.build(PT.Axiom(seq))))) seq) 
		     else cont (throw (Local Fail) seq)
		       
		 | _ -> failwith("All cases should have been covered!")
	     end
	       
	   | Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved, polar) when FSet.is_empty delta 
	       -> begin 
		 if (inloop||((FSet.is_empty gammaformP) && (FSet.is_empty gammaformPSaved))) 
		 then cont (throw (Local(Fail)) seq)
		 else

		   if (FSet.is_empty gammaformP) then 
		     let newseq = Seq.EntUF(gammatomN, FSet.empty, gammaformPSaved, gammaformPTried, FSet.empty, polar) in
		       cont (throw (Fake(false,!dir,Comp(lk_solve false newseq))) seq )

		   else
		     let rec action_analysis =
		       let intercept intercept_fun v cont =
			 let newcont loc_ans = 
			   let prune_ans = match loc_ans with
			     | Local a       -> Local a
			     | Fake(b1,b2,c) -> Fake(b1,b2)
			   in
			     match intercept_fun prune_ans with
			       | _    -> cont loc_ans
			 in
			   v newcont
		       in
			 function
		       	   | Focus(toFocus, inter_fun) ->  (* real focus *)
			       let newgammaformP = FSet.remove toFocus gammaformP in
			       let newnewgammaformP = FSet.union newgammaformP gammaformPTried in
			       let u1 = lk_solve true  (Seq.EntF (gammatomN, toFocus, newnewgammaformP,  FSet.add toFocus gammaformPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF(gammatomN, FSet.empty, newgammaformP, FSet.add toFocus gammaformPTried, gammaformPSaved, polar)) in
				 ou (intercept inter_fun u1) u2 (fun pt -> PT.build(PT.OnePre (seq, pt))) (fun pt -> pt) cont

			   | Cut(3,toCut, inter_fun1, inter_fun2)-> (*cut_3*)
			       let u1 = lk_solve false (Seq.EntF (gammatomN, toCut, gammaformP, gammaformPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF (gammatomN, FSet.add (Form.negation toCut) FSet.empty, gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				 et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.build(PT.TwoPre (seq, pt1,pt2))) cont
				   
			   | Cut(4,toCut, inter_fun1, inter_fun2) -> (*cut_4 *) 
			       begin 
				 match (polarity polar toCut) with
				     Neg -> ()
				   | _ -> failwith ("focus_pick should have produced Negat Formulae") 			       
			       end;
			       let p = Form.negation toCut in
				 if (FSet.is_in p gammaformP) then failwith("focus_pick has suggested a stupid cut4");
				 let u1 = lk_solve false (Seq.EntUF (gammatomN, FSet.add toCut FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)) in
				 let u2 = lk_solve false (Seq.EntUF (gammatomN, FSet.empty, FSet.add p gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				   et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.build(PT.TwoPre (seq, pt1,pt2))) cont

			   | Cut(7,toCut, inter_fun1, inter_fun2) -> (*cut_7*)
			       let u1 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add toCut FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)) in
			       let u2 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add (Form.negation toCut) FSet.empty, gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				 et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.build(PT.TwoPre (seq, pt1,pt2))) cont

			   | Polarise(l,b, inter_fun) ->
			       let u = lk_solve false (Seq.EntUF (gammatomN, FSet.empty, gammaformP, FSet.empty, gammaformPSaved, Pol.add l (if b then Neg else Pos) polar)) in
				 straight (intercept inter_fun u) (fun pt -> pt) cont

			   | Get(b1,b2) -> cont (Fake(b1,b2,Comp(lk_solve inloop seq)))

			   | _       -> failwith("focus_pick has suggested a stupid action")
		     in
		       Fake(AskFocus(seq,action_analysis))
	       end		
	       
	   | Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved, polar)
	     -> let (toDecompose,newdelta) = FSet.next delta in
	       begin
		 match (F.reveal toDecompose) with
		   | _ when ((polarity polar toDecompose) = Pos) ->
		       let u = 
			 if (!loop_detect&&((FSet.is_in toDecompose gammaformP)||(FSet.is_in toDecompose gammaformPSaved)))
			 then lk_solve inloop (Seq.EntUF (gammatomN, newdelta, gammaformP, FSet.empty,gammaformPSaved, polar))
			 else lk_solve false (Seq.EntUF (gammatomN, newdelta, FSet.add toDecompose gammaformP, FSet.empty,gammaformPSaved, polar))
		       in
			 straight u (fun pt->PT.build(PT.OnePre (seq, pt))) cont
			   
		   | Lit (b, f, tl) when ((polarity polar toDecompose) = Neg) ->
		       let u = 
			 if (!loop_detect&&ASet.is_in (not b, f,tl) gammatomN)
			 then lk_solve inloop (Seq.EntUF (gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved, polar))
			 else lk_solve false (Seq.EntUF (ASet.add (not b, f, tl) gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved, polar))
		       in
			 straight u (fun pt->PT.build(PT.OnePre (seq, pt))) cont

		   | Lit (b, f, tl) when ((polarity polar toDecompose) = Und) ->
		       (* print_string ("Hitting "^f^" or -"^f^" in asynchronous phase\n"); *)
		       straight 
			 (lk_solve false (Seq.EntUF (gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved, Pol.add f (if b then Neg else Pos) polar)))
			 (fun pt->PT.build(PT.OnePre (seq, pt)))
			 cont

		   | AndN (a1, a2) -> 
		       let u1 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a1 newdelta, gammaformP, FSet.empty, gammaformPSaved, polar)) in
		       let u2 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a2 newdelta, gammaformP, FSet.empty, gammaformPSaved, polar)) in
			 et u1 u2 (fun pt1 pt2 -> PT.build(PT.TwoPre (seq, pt1,pt2))) cont
			   
		   | OrN (a1, a2) -> 
		       straight 
			 (lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a1 (FSet.add a2 newdelta), gammaformP, FSet.empty, gammaformPSaved, polar)))
			 (fun pt->PT.build(PT.OnePre (seq, pt)))
			 cont

		   | _ -> failwith("All cases should have been covered!")
	       end
     		 
       let rec wrap f =
	 let fin = fun w -> print_endline(w^", with "
					  ^(string_of_int count.(0))^" Successes, "
					  ^(string_of_int count.(1))^" Fails, "
					  ^(string_of_int count.(2))^" Faked Successes, and "
					  ^(string_of_int count.(3))^" Faked Fails, "
					 );
	   count.(0) <- 0; count.(1) <- 0; count.(2) <- 0; count.(3) <- 0 ; count.(4) <- 0; dir := true
	 in
	 let inter = function
	   | Fake(b1,b2,Comp v)  -> dir:= not b2 ;
	       begin
		 if !debug>0 then print_endline("No more "
						^(if b1 then "Success" else "Failure")
						^" branch on the "
						^(if b2 then "right" else "left"))
	       end;
	       Fake(Stop(b1,b2,fun _ -> wrap v))
	   | Local(Success(p))   -> fin "Total Success"; Local(Success(p))
	   | Local(Fail)         -> fin "Total Failure"; Local(Fail)
	 in
	   f inter

       let machine seq = wrap (lk_solve false seq)

     end)
;;
