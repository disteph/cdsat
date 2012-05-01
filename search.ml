open Formulae;;
open Sequents;;
open Strategy;;

(* Activates debug mode (displays fails, etc) *)
let debug = 2;;
let failcount = ref 0;;
let tiredcount = ref 0;;
let successcount = ref 0;;

module ProofSearch =
  functor (US:UserStrategy) ->
    (struct

       include US
       module Seq = Sequent(F)(FSet)(ASet)
       module PT = ProofTree(F)(FSet)(ASet)
       module Ans = Answer(F)(FSet)(ASet)
	 
       let throwfail s = 
	 failcount := (!failcount)+1;
	 if debug>0  then 
	   if (((!failcount) mod 500) ==0) then 
	     begin
	       print_int(!failcount);
	       if debug==1 then print_endline(" Fail : "); 
	       if debug>1  then print_endline(" Fail : "^Seq.toString s);
	     end;
	 Ans.Fail(empty_zip);;

       let throwtired s a= 
	 tiredcount := (!tiredcount)+1;
	 if debug>0  then 
	   if (((!tiredcount) mod 500) ==0) then 
	     begin
	       print_int(!tiredcount);
	       if debug==1 then print_endline(" Tired : ");
	       if debug>1  then print_endline(" Tired : "^Seq.toString s);
	     end; 
	 Ans.Fail(add_left (a, s) empty_zip)
       ;;

       let throwsuccess s= 
	 successcount := (!successcount)+1;
	 if debug>0  then 
	   if (((!successcount) mod 500) ==0) then 
	     begin
	       print_int(!successcount);
	       if debug==1 then print_endline(" Success : ");
	       if debug>1  then print_endline(" Success : "^Seq.toString s);
	     end;
	 Ans.Success(PT.Axiom(s))
       ;;

       (*
	 Updating a continuation in order to merge previous tired points (x) with newly obtained tired points (y).
       *)

       let make_new_cont cont x = function
	   Ans.Success(prooftree) -> cont (Ans.Success(prooftree))
	 | Ans.Fail(y) -> cont (Ans.Fail (fusion_zip x y))
       ;;

       let rec addall gamma1 gamma2 =
	 if (FSet.is_empty gamma1) then gamma2 
	 else let (a,newgamma1) = FSet.next gamma1 in
	   addall newgamma1 (FSet.add a gamma2);;

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

       let rec lk_solve cont inloop = function
	   Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved) when FSet.is_empty delta 
	     -> begin 
               if (inloop||((FSet.is_empty gammaformP) && (FSet.is_empty gammaformPSaved))) 
	       then cont (throwfail (Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved))) 
	       else
		 if (FSet.is_empty gammaformP) then 
		   let newcont = function 
                       Ans.Success(prooftree) -> cont (Ans.Success (prooftree))
		     | Ans.Fail(x) -> Ans.Fail(x) in
		   let newseq = Seq.EntUF(gammatomN, FSet.empty, gammaformPSaved, FSet.empty, FSet.empty) in
		     cont (throwtired newseq (function () -> lk_solve newcont false newseq)) 
		 else
		   let toFocus = focus_pick(gammatomN, gammaformP, gammaformPTried, gammaformPSaved) in
		   let newgammaformP = FSet.remove toFocus gammaformP in
		   let newnewgammaformP = addall newgammaformP gammaformPTried in
		   let newcont = function
                       Ans.Success(prooftree)
		       -> cont (Ans.Success(PT.OnePre (Seq.EntUF(gammatomN, FSet.empty, gammaformP, gammaformPTried, gammaformPSaved), prooftree)))
		     | Ans.Fail(x) 
		       -> lk_solve (make_new_cont cont x) false (Seq.EntUF (gammatomN, FSet.empty, newgammaformP, FSet.add toFocus gammaformPTried, gammaformPSaved))  in
		     lk_solve newcont true (Seq.EntF (gammatomN, toFocus, newnewgammaformP, FSet.empty, FSet.add toFocus gammaformPSaved))
	     end

	 | Seq.EntUF(gammatomN, delta, gammaformP, gammaformPTried, gammaformPSaved)
	   -> let (toDecompose,newdelta) = FSet.next delta in
	     begin
               match (F.reveal toDecompose) with
		   Pos p -> let newcont = function
                       Ans.Success(prooftree) 
		       -> cont (Ans.Success(PT.OnePre (Seq.EntUF(gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved), prooftree)))
		     | Ans.Fail(x) 
		       -> cont (Ans.Fail(x)) in
		     if ((FSet.is_in toDecompose gammaformP)||(FSet.is_in toDecompose gammaformPSaved))
		     then lk_solve newcont inloop (Seq.EntUF (gammatomN, newdelta, gammaformP, FSet.empty,gammaformPSaved))
		     else lk_solve newcont false (Seq.EntUF (gammatomN, newdelta, FSet.add toDecompose gammaformP, FSet.empty,gammaformPSaved))

		 | Neg (NegAtom (f, tl)) -> let newcont = function 
                       Ans.Success(prooftree) -> cont (Ans.Success(PT.OnePre (Seq.EntUF(gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved), prooftree)))
		     | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		     if (ASet.is_in (f,tl) gammatomN)
		     then lk_solve newcont inloop (Seq.EntUF (gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved))
		     else lk_solve newcont false (Seq.EntUF (ASet.add (f, tl) gammatomN,newdelta, gammaformP, FSet.empty, gammaformPSaved))

		 | Neg (AndN (a1, a2)) -> 
		     let newcont1 = function
			 Ans.Success(prooftree1) 
			 -> let newcont2 = function
			     Ans.Success(prooftree2) 
			     -> cont (Ans.Success(PT.TwoPre (Seq.EntUF(gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved), prooftree1, prooftree2)))
			   | Ans.Fail(x) -> cont (Ans.Fail(x)) in
			   lk_solve newcont2 inloop (Seq.EntUF (gammatomN, FSet.add a2 newdelta, gammaformP, FSet.empty, gammaformPSaved))
			     
                       | Ans.Fail(x) -> cont (Ans.Fail(x)) in                 
                       lk_solve newcont1 inloop (Seq.EntUF (gammatomN, FSet.add a1 newdelta, gammaformP, FSet.empty, gammaformPSaved))
			 
		 | Neg (OrN (a1, a2)) -> 
		     let newcont = function
                       Ans.Success(prooftree) -> cont (Ans.Success(PT.OnePre (Seq.EntUF(gammatomN, delta, gammaformP, FSet.empty, gammaformPSaved), prooftree)))
		     | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		     lk_solve newcont inloop (Seq.EntUF (gammatomN, FSet.add a1 (FSet.add a2 newdelta), gammaformP, FSet.empty, gammaformPSaved))
	     end
	 | Seq.EntF(gammatomN, g, gammaformP, gammaformPTried, gammaformPSaved)   
           -> begin
	     match (F.reveal g) with
	       | Neg(a)
		 -> let newcont = function
		     Ans.Success(prooftree) 
		     -> cont (Ans.Success(PT.OnePre (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved), prooftree)))
		   | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		   lk_solve newcont inloop (Seq.EntUF (gammatomN, FSet.add g FSet.empty, gammaformP, FSet.empty, gammaformPSaved))
		     
	       | Pos(AndP(a1, a2))
		 -> let newcont1 = function
		     Ans.Success(prooftree1) -> let newcont2 = function
			 Ans.Success(prooftree2) 
			 -> cont (Ans.Success(PT.TwoPre (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved), prooftree1, prooftree2)))
		       | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		       lk_solve newcont2 inloop (Seq.EntF (gammatomN, a2, gammaformP, FSet.empty, gammaformPSaved))
		   | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		   lk_solve newcont1 inloop (Seq.EntF (gammatomN, a1, gammaformP, FSet.empty, gammaformPSaved))
		     
	       | Pos(OrP(a1, a2))
		 -> 
		   let (a1',a2') = if (side_pick(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved)) then (a1, a2) else (a2, a1) in
		   let newcont2 = function
		     Ans.Success(prooftree2) 
		     -> cont (Ans.Success(PT.OnePre (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved), prooftree2)))
		   | Ans.Fail(x) -> cont (Ans.Fail(x)) in
		 let newcont1 = function
		     Ans.Success(prooftree1) 
		     -> cont (Ans.Success(PT.OnePre (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved), prooftree1)))
		   | Ans.Fail(x) -> lk_solve (make_new_cont newcont2 x) inloop (Seq.EntF (gammatomN, a2', gammaformP, FSet.empty, gammaformPSaved)) in
		   lk_solve newcont1 inloop (Seq.EntF (gammatomN, a1', gammaformP, FSet.empty, gammaformPSaved))

	       | Pos(PosAtom(f, tl))
		 -> if (ASet.is_in (f, tl) gammatomN) 
		 then cont (throwsuccess (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved))) 
		 else cont (throwfail (Seq.EntF(gammatomN, g, gammaformP, FSet.empty, gammaformPSaved)))
	   end;;
       

       let rec solve toresurect = 
	 (*     print_endline(printzipper (function (y, z) -> (printseq z)) " " (toresurect));*)
	 if is_empty (toresurect) then 
	   begin
	     print_endline("Total failure, with "^(string_of_int !failcount)^" Fails and "^(string_of_int !tiredcount)^" Tireds");
	     Ans.Fail(empty_zip)
	   end
	 else if is_out(toresurect) then
	   solve(home toresurect)
	 else
	   let (c, (a, b)) = next(toresurect) in
	     if debug>0 then print_string("Je (re)prend maintenant la piste : ");
	     if debug==1 then print_endline("");
	     if debug>1 then print_endline(Seq.toString b);
	     match a() with
		 Ans.Success(prooftree) 
		 -> print_endline("Success, with "^(string_of_int !failcount)^" Fails and "^(string_of_int !tiredcount)^" Tireds");
		   Ans.Success(prooftree)
               | Ans.Fail(x) -> let newtoresurect = (fusion_zip c (zipend x)) in
		   solve(newtoresurect)
       ;;

end)
;;
