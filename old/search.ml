open Formulae;;
open Sequents;;
open Strategy;;

let debug = ref 0;;          (* Activates debug mode (displays fails, etc) *)
let loop_detect = ref true;; (* Activates loop detection *)

module ProofSearch =
  functor (US:UserStrategy) ->
    (struct
       
       include US
       module PF = PrintableFormula(F)
       module Seq = Sequent(F)(FSet)(ASet)
       module PT = ProofTree(F)(FSet)(ASet)
       module Ans = Answer(F)(FSet)(ASet)

       let count = [|0;0;0;0;0|]
       let dir = ref 1

       let throw ans s = 
	 let (index, word, modulo) =
	   match ans with
	   | Ans.LocalSuccess(prooftree) -> (0,"LocalSuccess",1000000)
	   | Ans.LocalFail               -> (1,"LocalFail",1000000) 
	   | Ans.Fake(0,f)               -> (4,"Escape",1)
	   | Ans.Fake(1,f)               -> (2,"FakeSuccess->right",1)
	   | Ans.Fake(-1,f)              -> (3,"FakeFail->right",1)
	   | Ans.Fake(2,f)               -> (2,"FakeSuccess->left",1)
	   | Ans.Fake(-2,f)              -> (3,"FakeFail->left",1)
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
	   | Ans.LocalSuccess(prooftree1)       -> cont (Ans.LocalSuccess(fseq1 prooftree1))
	   | Ans.LocalFail ->
	       let newcont2 = function
		 | Ans.LocalSuccess(prooftree2) -> cont (Ans.LocalSuccess(fseq2 prooftree2))
		 | Ans.LocalFail                -> cont (Ans.LocalFail)
		 | Ans.Fake(i,f2)               -> cont (Ans.Fake(i,ou (fun cc -> cc Ans.LocalFail) f2 fseq1 fseq2))
	       in
		 v2 newcont2
	   | Ans.Fake(-1,f1) ->
	       let newcont2 = function
		 | Ans.LocalSuccess(prooftree2) -> cont (Ans.LocalSuccess(fseq2 prooftree2))
		 | Ans.LocalFail                -> cont (Ans.Fake(-1,ou f1 (fun cc -> cc Ans.LocalFail) fseq1 fseq2))
		 | Ans.Fake(-2,f2)              -> ou f1 f2 fseq1 fseq2 cont
		 | Ans.Fake(i,f2)               -> cont (Ans.Fake(i,ou (fun cc -> cc (Ans.Fake(-1,f1))) f2 fseq1 fseq2))
	       in
		 v2 newcont2
	   | Ans.Fake(i,f1)                     -> cont (Ans.Fake(i,ou f1 v2 fseq1 fseq2))
	 in
	   v1 newcont1

       let rec et v1 v2 fseq cont =
	 let newcont1 = function
	   | Ans.LocalSuccess(prooftree1) ->
	       let newcont2 = function
		 | Ans.LocalSuccess(prooftree2) -> cont (Ans.LocalSuccess(fseq prooftree1 prooftree2))
		 | Ans.LocalFail                -> cont (Ans.LocalFail)
		 | Ans.Fake(i,f2)               -> cont (Ans.Fake(i,et (fun cc -> cc (Ans.LocalSuccess(prooftree1))) f2 fseq))
	       in
		 v2 newcont2
	   | Ans.LocalFail                      -> cont (Ans.LocalFail)
	   | Ans.Fake(1,f1) ->
	       let newcont2 = function
		 | Ans.LocalSuccess(prooftree2) -> cont (Ans.Fake(1,et f1 (fun cc -> cc (Ans.LocalSuccess(prooftree2))) fseq))
		 | Ans.LocalFail                -> cont (Ans.LocalFail)
		 | Ans.Fake(2,f2)               -> et f1 f2 fseq cont
		 | Ans.Fake(i,f2)               -> cont (Ans.Fake(i,et (fun cc -> cc (Ans.Fake(1,f1))) f2 fseq)) 
	       in
		 v2 newcont2
	   | Ans.Fake(i,f1)                     -> cont (Ans.Fake(i,et f1 v2 fseq))
	 in
	   v1 newcont1

       let rec straight v fseq cont =
	 let newcont = function
	   | Ans.LocalSuccess(prooftree) -> cont (Ans.LocalSuccess(fseq prooftree))
	   | Ans.LocalFail               -> cont (Ans.LocalFail)
	   | Ans.Fake(i,f)               -> cont (Ans.Fake(i,straight f fseq))
	 in
	   v newcont

       let invert =  function
	 | Ans.LocalSuccess(pt) -> Ans.Fake(-1,fun cc -> cc (Ans.LocalSuccess(pt)))
	 | Ans.LocalFail        -> Ans.Fake(1,fun cc -> cc (Ans.LocalFail))
	 | Ans.Fake(i,f)        -> Ans.Fake(-i,f)

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

       let rec lk_solve inloop seq cont =
	 (*	 print_endline("attack"^Seq.toString seq); *)
	 match seq with
	   | Seq.EntF(gammatomN, g, gammaformP, gammaformPSaved, polar)
             -> begin
	       match (F.reveal g) with
		 | _ when ((Seq.polarity polar g) <> Pos) ->
		     straight 
		       (lk_solve inloop (Seq.EntUF (gammatomN, FSet.add g FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)))
		       (fun pt->PT.OnePre (seq, pt))
		       cont
		       
		 | AndP(a1, a2) ->
		     let u1 = lk_solve inloop (Seq.EntF (gammatomN, a1, gammaformP, gammaformPSaved, polar)) in
		     let u2 = lk_solve inloop (Seq.EntF (gammatomN, a2, gammaformP, gammaformPSaved, polar)) in
		       et u1 u2 (fun pt1 pt2 -> PT.TwoPre (seq, pt1,pt2)) cont
			 
		 | OrP(a1, a2) -> 
		     let (a1',a2') = if side_pick(gammatomN, g, gammaformP, gammaformPSaved, polar) then (a1,a2) else (a2,a1) in
		     let u1 = lk_solve inloop (Seq.EntF (gammatomN, a1', gammaformP, gammaformPSaved, polar)) in
		     let u2 = lk_solve inloop (Seq.EntF (gammatomN, a2', gammaformP, gammaformPSaved, polar)) in
		     let fseq = fun pt -> PT.OnePre(seq,pt) in
		       ou u1 u2 fseq fseq cont

		 | Lit(b,f, tl) ->
		     if (ASet.is_in (b,f, tl) gammatomN) 
		     then cont (throw (Ans.LocalSuccess(PT.Axiom(seq))) seq) 
		     else cont (throw (Ans.LocalFail) seq)
		       
		 | _ -> failwith("All cases should have been covered!")
	     end
	       
	   | Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved, polar) when FSet.is_empty delta 
	       -> begin 
		 if (inloop||((FSet.is_empty gammaformP) && (FSet.is_empty gammaformPSaved))) 
		 then cont (throw (Ans.LocalFail) seq)
		 else

		   if (FSet.is_empty gammaformP) then 
		     let newseq = Seq.EntUF(gammatomN, FSet.empty, gammaformPSaved, gammaformPTried, FSet.empty, polar) in
		       cont (throw (Ans.Fake((-1)*(!dir),lk_solve false newseq)) seq )

		   else
		     let rec action_analysis =
		       let intercept intercept_fun v cont =
			 let newcont loc_ans = match intercept_fun loc_ans with
			       | Accept    -> cont loc_ans
			       | Refuse    -> cont (invert loc_ans)
			       | Action(a) -> action_analysis a
			 in
			   v newcont
		       in
			 function
		       	   | Focus(toFocus, inter_fun) ->  (* real focus *)
			       let newgammaformP = FSet.remove toFocus gammaformP in
			       let newnewgammaformP = FSet.union newgammaformP gammaformPTried in
			       let u1 = lk_solve true  (Seq.EntF (gammatomN, toFocus, newnewgammaformP,  FSet.add toFocus gammaformPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF(gammatomN, FSet.empty, newgammaformP, FSet.add toFocus gammaformPTried, gammaformPSaved, polar)) in
				 ou (intercept inter_fun u1) u2 (fun pt -> PT.OnePre (seq, pt)) (fun pt -> pt) cont

			   | Cut(3,toCut, inter_fun1, inter_fun2)-> (*cut_3*)
			       let u1 = lk_solve false (Seq.EntF (gammatomN, toCut, gammaformP, gammaformPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF (gammatomN, FSet.add (PF.negation toCut) FSet.empty, gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				 et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.TwoPre (seq, pt1,pt2)) cont
				   
			   | Cut(4,toCut, inter_fun1, inter_fun2) -> (*cut_4 *) 
			       begin 
				 match (Seq.polarity polar toCut) with
				     Neg -> ()
				   | _ -> failwith ("focus_pick should have produced Negat Formulae") 			       
			       end;
			       let p = PF.negation toCut in
				 if (FSet.is_in p gammaformP) then failwith("focus_pick has suggested a stupid cut4");
				 let u1 = lk_solve false (Seq.EntUF (gammatomN, FSet.add toCut FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)) in
				 let u2 = lk_solve false (Seq.EntUF (gammatomN, FSet.empty, FSet.add p gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				   et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.TwoPre (seq, pt1,pt2)) cont

			   | Cut(7,toCut, inter_fun1, inter_fun2) -> (*cut_7*)
			       let u1 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add toCut FSet.empty, gammaformP, FSet.empty, gammaformPSaved, polar)) in
			       let u2 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add (PF.negation toCut) FSet.empty, gammaformP, gammaformPTried, gammaformPSaved, polar)) in
				 et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (fun pt1 pt2 -> PT.TwoPre (seq, pt1,pt2)) cont

			   | Polarise(l,b, inter_fun) ->
			       let u = lk_solve false (Seq.EntUF (gammatomN, FSet.empty, gammaformP, FSet.empty, gammaformPSaved, Pol.add l (if b then Neg else Pos) polar)) in
				 straight (intercept inter_fun u) (fun pt -> pt) cont

			   | Fake(b) -> cont (Ans.Fake((if b then 1 else -1), lk_solve inloop seq))

			   | _       -> failwith("focus_pick has suggested a stupid action")
		     in
		       action_analysis (focus_pick(gammatomN, gammaformP, gammaformPTried, gammaformPSaved, polar))
	       end		
	       
	   | Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved, polar)
	     -> let (toDecompose,newdelta) = FSet.next delta in
	       begin
		 match (F.reveal toDecompose) with
		   | _ when ((Seq.polarity polar toDecompose) = Pos) ->
		       let u = 
			 if (!loop_detect&&((FSet.is_in toDecompose gammaformP)||(FSet.is_in toDecompose gammaformPSaved)))
			 then lk_solve inloop (Seq.EntUF (gammatomN, newdelta, gammaformP, FSet.empty,gammaformPSaved, polar))
			 else lk_solve false (Seq.EntUF (gammatomN, newdelta, FSet.add toDecompose gammaformP, FSet.empty,gammaformPSaved, polar))
		       in
			 straight u (fun pt->PT.OnePre (seq, pt)) cont
			   
		   | Lit (b, f, tl) when ((Seq.polarity polar toDecompose) = Neg) ->
		       let u = 
			 if (!loop_detect&&ASet.is_in (not b, f,tl) gammatomN)
			 then lk_solve inloop (Seq.EntUF (gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved, polar))
			 else lk_solve false (Seq.EntUF (ASet.add (not b, f, tl) gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved, polar))
		       in
			 straight u (fun pt->PT.OnePre (seq, pt)) cont

		   | Lit (b, f, tl) when ((Seq.polarity polar toDecompose) = Und) ->
		       (* print_string ("Hitting "^f^" or -"^f^" in asynchronous phase\n"); *)
		       straight 
			 (lk_solve false (Seq.EntUF (gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved, Pol.add f (if b then Neg else Pos) polar)))
			 (fun pt->PT.OnePre (seq, pt))
			 cont

		   | AndN (a1, a2) -> 
		       let u1 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a1 newdelta, gammaformP, FSet.empty, gammaformPSaved, polar)) in
		       let u2 = lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a2 newdelta, gammaformP, FSet.empty, gammaformPSaved, polar)) in
			 et u1 u2 (fun pt1 pt2 -> PT.TwoPre (seq, pt1,pt2)) cont
			   
		   | OrN (a1, a2) -> 
		       straight 
			 (lk_solve inloop (Seq.EntUF (gammatomN, FSet.add a1 (FSet.add a2 newdelta), gammaformP, FSet.empty, gammaformPSaved, polar)))
			 (fun pt->PT.OnePre (seq, pt))
			 cont

		   | _ -> failwith("All cases should have been covered!")
	       end
     		 
       let rec solve f =
	 let fin = fun w -> print_endline(w^", with "
					  ^(string_of_int count.(0))^" Successes, "
					  ^(string_of_int count.(1))^" Fails, "
					  ^(string_of_int count.(2))^" Faked Successes, and "
					  ^(string_of_int count.(3))^" Faked Fails, "
					 );
	   count.(0) <- 0; count.(1) <- 0; count.(2) <- 0; count.(3) <- 0 ; count.(4) <- 0; dir := 1;
	 in
	 let pasfin = fun w -> if !debug>0 then print_endline(w); solve 
	 in
	 let inter = function
	   | Ans.Fake(-1,v)  -> dir:=2 ; pasfin "top-level -1" v
	   | Ans.Fake(1,v)   -> dir:=2 ; pasfin "top-level 1" v 
	   | Ans.Fake(0,v)   -> pasfin "top-level 0" v 
	   | Ans.Fake(-2,v)  -> dir:=1 ; pasfin "top-level -2" v
	   | Ans.Fake(2,v)   -> dir:=1 ; pasfin "top-level 2" v 
	   | Ans.LocalSuccess(p)-> fin "Total Success"; Ans.Success(p)
	   | Ans.LocalFail      -> fin "Total Failure"; Ans.Fail
	 in
	   f inter

     end)
;;
