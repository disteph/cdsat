open Flags
open Formulae
open Collection
open Sequents
open Patricia

module ProofSearch (F: FormulaImplem) (FSet: CollectImplem with type e = F.t) (ASet: ACollectImplem) =
  (struct

     (* Loads the FrontEnd *)	   
     module FE = FrontEnd(F)(FSet)(ASet) 
     include FE
     
     (* Array where we count how many events we get *)

     let count = [|0;0;0;0;0;0|]

     (* Chooses whether, when faking failure, the natural
	behaviour is to go right (true) or left (false) *)

     let dir = ref true

     (* Throws a local answer ans on sequent s: printing message
	on standard output if debug mode is on, incrementing the
	adequate counter in the array *)

     let throw ans s= 
       let (index, word) = match ans with
	 | Local(Success(_)) -> (0,"LocalSuccess")
	 | Local(Fail(_))    -> (1,"LocalFail") 
	 | Fake(a,b,_)       -> let mydir = if b then "right" else "left" in
	     if a then (2,"FakeSuccess->"^mydir) else (3,"FakeFail->"^mydir)
       in
	 count.(index) <- count.(index) + 1;
	 if !debug>0  then 
	   if (((count.(index)) mod every.(index)) ==0) then
	     begin
	       print_int(count.(index));
	       if !debug==1 then print_endline(" "^word^": ");
	       if !debug>1  then print_endline(" "^word^": "^(print_state s));
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
	 | (Seq.EntUF(_, delta, _, _, polar),(atomN',formP'::formPSaved'::[])) -> 
	     Seq.EntUF(atomN', delta, formP', formPSaved', polar)
	 | (_,(_,l)) -> failwith("relevant - Wrong number of arguments: "^(string_of_int(List.length l)))
     else seq
       
     let stdtwo seq = fun pt1 pt2 -> PT.build(PT.TwoPre (relevant(seq,ext [pt1;pt2]), pt1,pt2))
     let stdone seq = fun pt      -> PT.build(PT.OnePre (relevant(seq,ext [pt]), pt))

     let prune = function
       | Local a       -> Local a
       | Fake(b1,b2,c) -> Fake(b1,b2)

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

     let rec lk_solve inloop seq data cont =
       

       match seq with
	 | Seq.EntF(atomN, g, formP, formPSaved, polar)
           -> begin match (F.reveal g) with
	     | _ when ((polarity polar g) <> Pos) ->
		 straight 
		   (lk_solve inloop (Seq.EntUF (atomN, FSet.add g FSet.empty, formP, formPSaved, polar)) data)
		   (stdone seq) seq cont
		   
	     | AndP(a1, a2) ->
		 let u1 = lk_solve inloop (Seq.EntF (atomN, a1, formP, formPSaved, polar)) data in
		 let u2 = lk_solve inloop (Seq.EntF (atomN, a2, formP, formPSaved, polar)) data in
		   et u1 u2 (stdtwo seq) seq cont
		     
	     | OrP(a1, a2) -> 
		 let side_pick b = 
		   let (a1',a2') = if b then (a1,a2) else (a2,a1) in
		   let u1 = lk_solve inloop (Seq.EntF (atomN, a1', formP, formPSaved, polar)) data in
		   let u2 = lk_solve inloop (Seq.EntF (atomN, a2', formP, formPSaved, polar)) data in
		     ou u1 u2 (stdone seq) (stdone seq) seq cont
		 in
		   Fake(AskSide(seq,side_pick,data))

	     | Lit t ->
		 let (b,p,_) = Atom.reveal t in
		 let rec filt_inspect filtered cont =
		   if ASet.is_empty filtered then cont (throw (Local(Fail seq)) seq)
		   else
		     let (at,newfiltered) = ASet.next filtered in
		     let u2 = filt_inspect newfiltered in
		       if (Atom.equal at t) then 
			 cont (throw (Local(Success(PT.build(PT.Axiom(
							       relevant(seq,(ASet.add t ASet.empty,FSet.empty::FSet.empty::[]))
							     ))))) seq)
		       else u2 cont
		 in filt_inspect (ASet.filter b p atomN) cont

	     | _ -> failwith("All cases should have been covered!")
	   end
	     
	 | Seq.EntUF(atomN, delta, formP, formPSaved, polar) when not (FSet.is_empty delta)
	     -> let (toDecompose,newdelta) = FSet.next delta in
	       begin match (F.reveal toDecompose) with
		 | _ when ((polarity polar toDecompose) = Pos) ->
		     if (!loop_detect&&((FSet.is_in toDecompose formP)||(FSet.is_in toDecompose formPSaved)))
		     then let u = lk_solve inloop (Seq.EntUF (atomN, newdelta, formP, formPSaved, polar)) data in
		       straight u (stdone seq) seq cont
		     else let u = lk_solve false (Seq.EntUF (atomN, newdelta, FSet.add toDecompose formP, formPSaved, polar)) data in
		       straight u (fun pt->PT.build(PT.OnePre (relevant(seq,match ext [pt] with
									  | (ga,gfP::l) -> (ga,
											    (if FSet.is_in toDecompose gfP
											     then FSet.remove toDecompose gfP
											     else gfP)::l)
									  | _           -> failwith("Should not happen")), pt))) seq cont
			 
		 | Lit t when ((polarity polar toDecompose) = Neg) -> let t' = (Atom.negation t) in
		     if (!loop_detect&&ASet.is_in t' atomN)
		     then let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar)) data in
		       straight u (stdone seq) seq cont
		     else let u = lk_solve false (Seq.EntUF (ASet.add t' atomN,newdelta, formP, formPSaved, polar)) data in
		       straight u (fun pt->PT.build(PT.OnePre (relevant(seq,
									let (ga,l) = ext [pt] in 
									  ((if ASet.is_in t' ga then ASet.remove t' ga else ga),l))
								 , pt))) seq cont

		 | Lit t when ((polarity polar toDecompose) = Und) -> let (b,f,_)=Atom.reveal t in
		     (* print_string ("Hitting "^f^" or -"^f^" in asynchronous phase\n"); *)
		     straight 
		       (lk_solve false (Seq.EntUF (atomN, delta, formP, formPSaved, Pol.add f (if b then Neg else Pos) polar)) data)
		       (fun pt->pt) seq cont

		 | AndN (a1, a2) -> 
		     let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 newdelta, formP, formPSaved, polar)) data in
		     let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a2 newdelta, formP, formPSaved, polar)) data in
		       et u1 u2 (stdtwo seq) seq cont
			 
		 | OrN (a1, a2) -> 
		     straight 
		       (lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 (FSet.add a2 newdelta), formP, formPSaved, polar)) data)
		       (stdone seq) seq cont

		 | _ -> failwith("All cases should have been covered!")
	       end

	 | Seq.EntUF(atomN, _, formP, formPSaved, polar) 
	   ->begin if !Flags.debug>1 then print_endline("attack "^print_state seq);
	     let rec lk_solvef formPChoose formP formPSaved actionO data cont = 

	       if ((FSet.is_empty formPChoose) && (FSet.is_empty formPSaved)) 
	       then cont (throw (Local(Fail seq)) seq)
	       else

		 let rec action_analysis =
		   let intercept inter_fun v cont =
		     let newcont loc_ans = match inter_fun (prune loc_ans) with
		       | Action(f) -> action_analysis f
		       | _         -> cont loc_ans
		     in v newcont
		   in function
		     | Focus(toFocus,inter_fun,l) ->  count.(4)<-count.(4)+1;(* real focus *)
			 (* print_endline(Form.toString toFocus);*)
			 if not (FSet.is_in toFocus formPChoose) then failwith("Not allowed to focus on this, you are cheating, you naughty!!!")
			 else
			   let u1 = lk_solve true  (Seq.EntF (atomN, toFocus, FSet.remove toFocus formP, FSet.add toFocus formPSaved, polar)) data in
			   let u2 = lk_solvef (FSet.remove toFocus formPChoose) formP formPSaved l data in
			     ou (intercept inter_fun u1) u2
			       (fun pt -> PT.build(PT.OnePre (relevant(seq,match ext [pt] with
									 | (ga,gfP::l) -> (ga,(FSet.add toFocus gfP)::l)
									 | _           -> failwith("Should not happen"))  , pt))) 
			       (fun pt -> pt) seq cont
			       
		     | Cut(3,toCut, inter_fun1, inter_fun2,l)-> (*cut_3*)
			 count.(5)<-count.(5)+1; 
			 if !Flags.debug>1&& count.(5) mod Flags.every.(6)=0 then print_endline("Cut3 on "^Form.toString toCut);
			 let u1 = lk_solve true (Seq.EntF (atomN, toCut, formP, formPSaved, polar)) data in
			 let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar)) data in
			 let u3 = lk_solvef formPChoose formP formPSaved l data in
			   ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (stdtwo seq) seq) u3 (fun pt -> pt) (fun pt -> pt) seq cont
			     
		     | Cut(7,toCut, inter_fun1, inter_fun2,l) -> (*cut_7*)
			 count.(5)<-count.(5)+1; 
			 if !Flags.debug>1&& count.(5) mod Flags.every.(6)=0 then print_endline("Cut7 on "^Form.toString toCut);
			 let u1 = lk_solve true (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, formPSaved, polar)) data in
			 let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar)) data in
			 let u3 = lk_solvef formPChoose formP formPSaved l data in
			   ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (stdtwo seq) seq) u3 (fun pt -> pt) (fun pt -> pt) seq cont

		     | Polarise(l,b, inter_fun) ->
			 let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved, Pol.add l (if b then Neg else Pos) polar)) data in
			   straight (intercept inter_fun u) (fun pt -> pt) seq cont

		     | Get(b1,b2,l) -> cont (Fake(b1,!dir=b2,Comp(lk_solvef formPChoose formP formPSaved l data)))

		     | Search(tosearch,inter_fun,A l) ->
			 begin match tosearch false seq with
			   | A(a) -> if !Flags.debug>1 then print_endline("Found previous success/failure, looking for exact on "^print_state seq);
			       intercept inter_fun (fun cc->cc (throw (Local(a)) seq)) cont
			   | _    -> if !Flags.debug>1 then print_endline("Found no previous success/failure, looking for exact on "^print_state seq);
			       lk_solvef formPChoose formP formPSaved l data cont
			 end

		     | Search(tosearch,inter_fun,F f) ->
			 begin match tosearch true seq with
			   | A(a) -> if !Flags.debug>1 then print_endline("Found previous success or failure (looking for approx as well) on "^print_state seq);
			       intercept inter_fun (fun cc->cc (throw (Local(a)) seq)) cont
			   | F(d1,d2) ->
			       if !Flags.debug>1
			       then print_endline(if not(ASet.is_empty d1) then "Found approx. in atoms on "^print_state seq
						  else if not(FSet.is_empty d2) then "Found approx. in pos form on "^print_state seq
						  else "Found no approx on "^print_state seq);
			       lk_solvef formPChoose formP formPSaved (f (d1,d2)) data cont
			 end

		     | Restore l ->
			 lk_solvef (FSet.union formPChoose formPSaved) (FSet.union formP formPSaved) FSet.empty l data cont

		     | _       -> failwith("focus_pick has suggested a stupid action")

		 in match actionO with
		   | Some(action)-> action_analysis action
		   | None        -> Fake(AskFocus(seq,formPChoose,action_analysis,data))
	     in
	     let newcont inter_fun loc_ans =
	       let recept_analysis = function _ -> cont loc_ans in
		 match inter_fun (prune loc_ans) with
		   | Exit(recept)     -> recept_analysis recept
		   | Mem(tomem,recept)-> (begin match loc_ans with
					    | Local(a) -> tomem a
					    | Fake _   -> ()
					  end;  recept_analysis recept)
	     in
	     let notify_analysis (accept_defeat,newdata,inter_fun,action0) =
	       if (inloop&&accept_defeat) then cont (throw (Local(Fail seq)) seq)
	       else lk_solvef formP formP formPSaved action0 newdata (newcont inter_fun)
	     in Fake(Notify(seq,inloop,notify_analysis,data))
		  
	   end
	     

     (* Wraps the above function by providing top-level continuation
	inter (for interaction with user), and printing a couple of
	messages for standard output *)

     let rec wrap f =
       let fin = fun w -> (print_endline(w^", with "
					 ^(string_of_int count.(0))^" Successes, "
					 ^(string_of_int count.(1))^" Fails, "
					 ^(string_of_int count.(2))^" Faked Successes, and "
					 ^(string_of_int count.(3))^" Faked Fails, "
					 ^(string_of_int count.(4))^" Focus, "
					 ^(string_of_int count.(5))^" Cuts, "
					);
			   for i=0 to Array.length count-1 do count.(i) <- 0 done;
			   dir := true)
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

     let machine seq data = wrap (lk_solve false seq data)

   end: sig
     module FE : (FrontEndType with module F=F and module FSet=FSet and module ASet=ASet)
     val machine : FE.Seq.t -> 'a ->'a FE.output
   end)
