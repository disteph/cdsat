open Flags
open Formulae;;
open Collection;;
open Sequents;;
open Patricia;;

module ProofSearch =
  functor (F: FormulaImplem) ->
    functor (FSet: CollectImplem with type e = F.t) ->
      functor (ASet: CollectImplem with type e = Atom.t) ->
	(struct

	   (* Loads the FrontEnd *)	   
	   module FE = FrontEnd(F)(FSet)(ASet) 
	   include FE
	   
	   (* Array where we count how many events we get *)

	   let count = [|0;0;0;0;0|]

	   (* Chooses whether, when faking failure, the natural
	   behaviour is to go right (true) or left (false) *)

	   let dir = ref true

	   (* Throws a local answer ans on sequent s: printing message
	      on standard output if debug mode is on, incrementing the
	      adequate counter in the array *)

	   let throw ans s= 
	     let (index, word, modulo) =
	       match ans with
		 | Local(Success(_)) -> (0,"LocalSuccess",100000)
		 | Local(Fail(_))    -> (1,"LocalFail",100000) 
		 | Fake(a,b,_)       -> let mydir = if b then "right" else "left" in
		     if a then (2,"FakeSuccess->"^mydir,1) else (3,"FakeFail->"^mydir,1)
	     in
	       count.(index) <- count.(index) + 1;
	       if !debug>0  then 
		 if (((count.(index)) mod modulo) ==0) then
		   begin
		     print_int(count.(index));
		     if !debug==1 then print_endline(" "^word^": ");
		     if !debug>1  then print_endline(" "^word^": "^(Seq.toString s));
		   end; 
	       ans

	   (* Combines two computations in OR style (with backtrack
	      management): success = success for either computation *)

	   let rec ou v1 v2 fseq1 fseq2 seq cont =
	     let newcont1 = function
	       | Local(Success prooftree1)       -> cont (Local(Success(fseq1 prooftree1)))
	       | Local(Fail seq1) ->
		   let newcont2 = function
		     | Local(Success(prooftree2))-> cont (Local(Success(fseq2 prooftree2)))
		     | Local(Fail _)             -> cont (Local(Fail seq))
		     | Fake(b1,b2,Comp f2)       -> cont (Fake(b1,b2,Comp(ou (fun cc -> cc (Local(Fail seq1))) f2 fseq1 fseq2 seq)))
		   in
		     v2 newcont2
	       | Fake(false,true,Comp f1) ->
		   let newcont2 = function
		     | Local(Success prooftree2) -> cont (Local(Success(fseq2 prooftree2)))
		     | Local(Fail seq2)          -> cont (Fake(false,true,Comp(ou f1 (fun cc -> cc (Local(Fail seq2))) fseq1 fseq2 seq)))
		     | Fake(false,false,Comp f2) -> ou f1 f2 fseq1 fseq2 seq cont
		     | Fake(b1,b2,Comp f2)       -> cont (Fake(b1,b2,Comp(ou (fun cc -> cc (Fake(false,true,Comp f1))) f2 fseq1 fseq2 seq)))
		   in
		     v2 newcont2
	       | Fake(b1,b2,Comp f1)             -> cont (Fake(b1,b2,Comp(ou f1 v2 fseq1 fseq2 seq)))
	     in
	       v1 newcont1

	   (* Combines two computations in AND style (with backtrack
	      management): success = success for both computations *)

	   let rec et v1 v2 fseq seq cont =
	     let newcont1 = function
	       | Local(Success prooftree1) ->
		   let newcont2 = function
		     | Local(Success prooftree2)-> cont (Local(Success(fseq prooftree1 prooftree2)))
		     | Local(Fail _)            -> cont (Local(Fail seq))
		     | Fake(b1,b2,Comp f2)      -> cont (Fake(b1,b2,Comp(et (fun cc -> cc (Local(Success prooftree1))) f2 fseq seq)))
		   in
		     v2 newcont2
	       | Local(Fail _)                  -> cont (Local(Fail seq))
	       | Fake(true,true,Comp f1) ->
		   let newcont2 = function
		     | Local(Success prooftree2)-> cont (Fake(true,true,Comp(et f1 (fun cc -> cc (Local(Success prooftree2))) fseq seq)))
		     | Local(Fail _)            -> cont (Local(Fail seq))
		     | Fake(true,false,Comp f2) -> et f1 f2 fseq seq cont
		     | Fake(b1,b2,Comp f2)      -> cont (Fake(b1,b2,Comp(et (fun cc -> cc (Fake(true,true,Comp f1))) f2 fseq seq)))
		   in
		     v2 newcont2
	       | Fake(b1,b2,Comp f1)            -> cont (Fake(b1,b2,Comp(et f1 v2 fseq seq)))
	     in
	       v1 newcont1

	   (* Unary version of ou and and, for homogeneous style *)

	   let rec straight v fseq seq cont =
	     let newcont = function
	       | Local(Success prooftree)-> cont (Local(Success(fseq prooftree)))
	       | Local(Fail _)           -> cont (Local(Fail seq))
	       | Fake(b1,b2,Comp f)      -> cont (Fake(b1,b2,Comp(straight f fseq seq)))
	     in
	       v newcont


	   (* Functions used to prune the generated proof-trees from useless formulae:
	      relevant prunes sequent seq from the formulae not present in datastructure d
	      update *)

	   let rec ext = function
	     | [] -> (ASet.empty,[])
	     | p::l ->
		 let (a,b) = Seq.interesting (PT.conclusion p) in
		 let (a',b') = ext l in
		   (ASet.union a a',
		    let rec union_list = function
		      | l,[] -> l
		      | (f1::l1),(f2::l2) -> (FSet.union f1 f2)::(union_list(l1,l2))
		      | _ -> failwith("Lists of different lengths")
		    in union_list (b,b'))

	   let relevant (seq,d) = if !weakenings then
	     match (seq,d) with
	       | (Seq.EntF(_, g, _, _, polar),(atomN',formP'::formPSaved'::[])) ->
		   Seq.EntF(atomN', g, formP', formPSaved', polar)
	       | (Seq.EntUF(_, delta, _, _, _, polar),(atomN',formP'::formPSaved'::formPTried'::[])) -> 
		   Seq.EntUF(atomN', delta, formP', formPTried', formPSaved', polar)
	       | (_,(_,l)) -> failwith("relevant - Wrong number of arguments: "^(string_of_int(List.length l)))
	   else seq
 
	     
       (*
	* Main Search function 
	* delta = Formulae to be asynchronously decomposed 
	* atomN = negative atoms found in asynchronous phase (negation symbol not stored)
	* formP = positive formulae found in asynchronous phase (focus to be placed on them later)
	* formPTried = positive formulae found in asynchronous phase (focus on them has failed)
	* formPSaved = positive formulae on which focus has been placed "more times" than remaining formulae in formP 
	* (These other formulae have priority for focus -> ensures fairness)
	* Returns Success(Prooftree) if a proof is found
	*)

       let rec lk_solve inloop seq cont =
	 (*	 print_endline("attack"^Seq.toString seq); *)

	 let stdtwo = fun pt1 pt2 -> PT.build(PT.TwoPre (relevant(seq,ext [pt1;pt2]), pt1,pt2)) in
	 let stdone = fun pt->PT.build(PT.OnePre (relevant(seq,ext [pt]), pt)) in
	 match seq with
	   | Seq.EntF(atomN, g, formP, formPSaved, polar)
             -> begin
	       match (F.reveal g) with
		 | _ when ((polarity polar g) <> Pos) ->
		     straight 
		       (lk_solve inloop (Seq.EntUF (atomN, FSet.add g FSet.empty, formP, FSet.empty, formPSaved, polar)))
		       (fun pt->PT.build(PT.OnePre (relevant(seq,match ext [pt] with
							       | (ga,gfP::gfPS::gfPT::[]) -> (ga,gfP::gfPS::[])
							       | _ -> failwith("Should not happen")), pt)))
		       seq cont
		       
		 | AndP(a1, a2) ->
		     let u1 = lk_solve inloop (Seq.EntF (atomN, a1, formP, formPSaved, polar)) in
		     let u2 = lk_solve inloop (Seq.EntF (atomN, a2, formP, formPSaved, polar)) in
		       et u1 u2 stdtwo seq cont
			 
		 | OrP(a1, a2) -> 
		     let side_pick b = 
		       let (a1',a2') = if b then (a1,a2) else (a2,a1) in
		       let u1 = lk_solve inloop (Seq.EntF (atomN, a1', formP, formPSaved, polar)) in
		       let u2 = lk_solve inloop (Seq.EntF (atomN, a2', formP, formPSaved, polar)) in
		       let fseq = fun pt -> PT.build(PT.OnePre(relevant(seq,ext [pt]),pt)) in
			 ou u1 u2 fseq fseq seq cont
		     in
		       Fake(AskSide(seq,side_pick))

		 | Lit t ->
		     if (ASet.is_in t atomN) 
		     then cont (throw (Local(Success(PT.build(PT.Axiom(
								relevant(seq,(ASet.add t ASet.empty,FSet.empty::FSet.empty::[]))
							      ))))) seq)
		     else cont (throw (Local(Fail seq)) seq)
		       
		 | _ -> failwith("All cases should have been covered!")
	     end
	       
	   | Seq.EntUF(atomN, delta, formP, formPTried, formPSaved, polar) when FSet.is_empty delta 
	       -> begin 
		 if (inloop||((FSet.is_empty formP) && (FSet.is_empty formPSaved))) 
		 then cont (throw (Local(Fail seq)) seq)
		 else

		   if (FSet.is_empty formP) then 
		     let newseq = Seq.EntUF(atomN, FSet.empty, formPSaved, formPTried, FSet.empty, polar) in
		       cont (throw (Fake(false,!dir,Comp(lk_solve false newseq))) seq )

		   else
		     let rec action_analysis =
		       let intercept intercept_fun cont loc_ans =
			 let prune_ans = match loc_ans with
			   | Local a       -> Local a
			   | Fake(b1,b2,c) -> Fake(b1,b2)
			 in
			 let rec recept_analysis = function
			   | Mem(tomem,recept) -> 
			       begin match loc_ans with
				 | Local(a) -> tomem a
				 | Fake _   -> ()
			       end ; recept_analysis recept
			   | Action(f) -> 
			       action_analysis f
			   | _    -> 
			       cont loc_ans
			 in recept_analysis(intercept_fun prune_ans)
		       in
			 function
		       	   | Focus(toFocus, inter_fun) ->  (* real focus *)
			       let newformP = FSet.remove toFocus formP in
			       let newnewformP = FSet.union newformP formPTried in
			       let u1 = lk_solve true  (Seq.EntF (atomN, toFocus, newnewformP,  FSet.add toFocus formPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF(atomN, FSet.empty, newformP, FSet.add toFocus formPTried, formPSaved, polar)) in
				 ou u1 u2
				   (fun pt -> PT.build(PT.OnePre (relevant(seq,match ext [pt] with
									       | (ga,gfP::gfPS::[]) ->
										   (ga,
										    FSet.add toFocus (FSet.inter gfP newformP)
										    ::(FSet.inter gfPS formPSaved)
										    ::(FSet.inter gfP formPTried)::[])
									       | _ -> failwith("Should not happen"))
								    , pt))) 
				   (fun pt -> pt) seq (intercept inter_fun cont)
				   
			   | Cut(3,toCut, inter_fun1, inter_fun2)-> (*cut_3*)
			       let u1 = lk_solve false (Seq.EntF (atomN, toCut, formP, formPSaved, polar)) in
			       let u2 = lk_solve false (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPTried, formPSaved, polar)) in
				 et (fun cc->u1(intercept inter_fun1 cc)) (fun cc->u2(intercept inter_fun2 cc)) stdtwo seq cont
				   
			   | Cut(4,toCut, inter_fun1, inter_fun2) -> (*cut_4 *) 
			       begin 
				 match (polarity polar toCut) with
				     Neg -> ()
				   | _ -> failwith ("focus_pick should have produced Negat Formulae") 			       
			       end;
			       let p = Form.negation toCut in
				 if (FSet.is_in p formP) then failwith("focus_pick has suggested a stupid cut4");
				 let u1 = lk_solve false (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, FSet.empty, formPSaved, polar)) in
				 let u2 = lk_solve false (Seq.EntUF (atomN, FSet.empty, FSet.add p formP, formPTried, formPSaved, polar)) in
				   et (fun cc->u1(intercept inter_fun1 cc)) (fun cc->u2(intercept inter_fun2 cc)) stdtwo seq cont

			   | Cut(7,toCut, inter_fun1, inter_fun2) -> (*cut_7*)
			       let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, FSet.empty, formPSaved, polar)) in
			       let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPTried, formPSaved, polar)) in
				 et (fun cc->u1(intercept inter_fun1 cc)) (fun cc->u2(intercept inter_fun2 cc)) stdtwo seq cont

			   | Polarise(l,b, inter_fun) ->
			       let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, FSet.empty, formPSaved, Pol.add l (if b then Neg else Pos) polar)) in
				 straight (fun cc->u(intercept inter_fun cc)) (fun pt -> pt) seq cont

			   | Get(b1,b2) -> cont (Fake(b1,b2,Comp(lk_solve inloop seq)))

			   | Search(tosearch, inter_fun) -> begin match tosearch seq with
			       | Some(a) -> intercept inter_fun cont (throw (Local(a)) seq)
			       | None    -> intercept inter_fun cont (Fake(false,!dir,Comp(lk_solve false seq)))
			     end

			   | _       -> failwith("focus_pick has suggested a stupid action")
		     in
		       Fake(AskFocus(seq,action_analysis))
	       end		
	       
	   | Seq.EntUF(atomN, delta, formP, formPTried, formPSaved, polar)
	     -> let (toDecompose,newdelta) = FSet.next delta in
	       begin
		 match (F.reveal toDecompose) with
		   | _ when ((polarity polar toDecompose) = Pos) ->
		       if (!loop_detect&&((FSet.is_in toDecompose formP)||(FSet.is_in toDecompose formPSaved)))
		       then let u = lk_solve inloop (Seq.EntUF (atomN, newdelta, formP, FSet.empty,formPSaved, polar)) in
		         straight u stdone seq cont
		       else let u = lk_solve false (Seq.EntUF (atomN, newdelta, FSet.add toDecompose formP, FSet.empty,formPSaved, polar)) in
		         straight u (fun pt->PT.build(PT.OnePre (relevant(seq,match ext [pt] with
									       | (ga,gfP::l) -> (ga,
												 (if FSet.is_in toDecompose gfP
												  then FSet.remove toDecompose gfP
												  else gfP)::l)
									       | _           -> failwith("Should not happen")), pt))) seq cont
			   
		   | Lit t when ((polarity polar toDecompose) = Neg) -> let t' = (Atom.negation t) in
		       if (!loop_detect&&ASet.is_in t' atomN)
		       then let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, FSet.empty, formPSaved, polar)) in
			 straight u stdone seq cont
		       else let u = lk_solve false (Seq.EntUF (ASet.add t' atomN,newdelta, formP, FSet.empty, formPSaved, polar)) in
			 straight u (fun pt->PT.build(PT.OnePre (relevant(seq,
									  let (ga,l) = ext [pt] in 
									    ((if ASet.is_in t' ga then ASet.remove t' ga else ga),l))
								   , pt))) seq cont

		   | Lit t when ((polarity polar toDecompose) = Und) -> let (b,f,_)=Atom.reveal t in
		       (* print_string ("Hitting "^f^" or -"^f^" in asynchronous phase\n"); *)
		       straight 
			 (lk_solve false (Seq.EntUF (atomN, delta, formP, FSet.empty, formPSaved, Pol.add f (if b then Neg else Pos) polar)))
			 stdone seq cont

		   | AndN (a1, a2) -> 
		       let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 newdelta, formP, FSet.empty, formPSaved, polar)) in
		       let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a2 newdelta, formP, FSet.empty, formPSaved, polar)) in
			 et u1 u2 stdtwo seq cont
			   
		   | OrN (a1, a2) -> 
		       straight 
			 (lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 (FSet.add a2 newdelta), formP, FSet.empty, formPSaved, polar)))
			 stdone seq cont

		   | _ -> failwith("All cases should have been covered!")
	       end

       (* Wraps the above function by providing top-level continuation
       inter (for interaction with user), and printing a couple of
       messages for standard output *)

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
	   | Local(Fail(s))      -> fin "Total Failure"; Local(Fail(s))
	 in
	   f inter

       (* Wraps the above function by providing initial inloop and
       initial sequent *)

       let machine seq = wrap (lk_solve false seq)

	 end: sig
           module FE : (FrontEndType with module F=F and module FSet=FSet and module ASet=ASet)
           val machine : FE.Seq.t -> FE.output
         end)
;;
