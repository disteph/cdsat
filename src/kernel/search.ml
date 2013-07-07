open Formulae
open Interfaces
open Sequents

module ProofSearch (MyTheory: DecProc)
  (F: FormulaImplem    with type lit = MyTheory.Atom.t)
  (FSet: CollectImplem with type e = F.t)
  (ASet: CollectImplem with type e = MyTheory.Atom.t)
  =
  (struct

     (* Loads the decision procedures *)
    module DecProc = MyTheory.Consistency(ASet)

     (* Loads the FrontEnd *)
    module FE = FrontEnd(MyTheory.Atom)(F)(FSet)(ASet)
    open FE
    

     (* Chooses whether, when faking failure, the natural
	behaviour is to go right (true) or left (false) *)

    let dir = ref true

     (* Throws a local answer ans on sequent s: printing message
	on standard output if debug mode is on, incrementing the
	adequate counter in the array *)

    let throw ans =
      let (index, word) = match ans with
	| Success _ -> (0,"LocalSuccess")
	| Fail _    -> (1,"LocalFail") 
      in
      Dump.Kernel.incr_count index;
      if !Flags.debug>0
      then (Dump.msg
              (Some (string_of_int (Dump.Kernel.read_count index)^" "^word^": "))
              (Some (string_of_int (Dump.Kernel.read_count index)^" "^word^": "^(Seq.toString (sequent ans))))
              (Some index));
      ans

     (* Combines two computations in OR style (with backtrack
	management): success = success for either computation *)

    let rec ou v1 v2 success1 success2 seq cont = Dump.Kernel.incr_branches();
      let newcont1 ans = Dump.Kernel.decr_branches(); match ans with
	| Local(Success(seq1,pt1))     -> cont(Local(success1(seq1,pt1)))
	| Local(Fail seq1) as u1 ->
	  let newcont2 = function
	    | Local(Success(seq2,pt2)) -> cont(Local(success2(seq2,pt2)))
	    | Local(Fail _)            -> cont(Local(Fail seq))
	    | Fake(b1,b2,Comp f2)      -> cont(Fake(b1,b2,Comp(ou (fun cc -> cc u1) f2 success1 success2 seq)))
	  in
	  v2 newcont2
	| Fake(false,true,Comp f1) as u1 ->
	  let newcont2 = function
	    | Local(Success(seq2,pt2)) -> cont(Local(success2(seq2,pt2)))
	    | Local(Fail seq2)         -> cont(Fake(false,true,Comp(ou f1 (fun cc -> cc (Local(Fail seq2))) success1 success2 seq)))
	    | Fake(false,false,Comp f2)-> ou f1 f2 success1 success2 seq cont
	    | Fake(b1,b2,Comp f2)      -> cont(Fake(b1,b2,Comp(ou (fun cc -> cc u1) f2 success1 success2 seq)))
	  in
	  v2 newcont2
	| Fake(b1,b2,Comp f1)          -> cont (Fake(b1,b2,Comp(ou f1 v2 success1 success2 seq)))
      in
      v1 newcont1

     (* Combines two computations in AND style (with backtrack
	management): success = success for both computations *)

    let rec et v1 v2 success seq cont = Dump.Kernel.incr_branches();
      let newcont1 ans = Dump.Kernel.decr_branches(); match ans with
	| Local(Success(seq1,pt1)) as u1 ->
	  let newcont2 = function
	    | Local(Success(seq2,pt2)) -> cont(Local(success(seq1,pt1)(seq2,pt2)))
	    | Local(Fail _)            -> cont(Local(Fail seq))
	    | Fake(b1,b2,Comp f2)      -> cont(Fake(b1,b2,Comp(et (fun cc -> cc u1) f2 success seq)))
	  in
	  v2 newcont2
	| Local(Fail _)                -> cont(Local(Fail seq))
	| Fake(true,true,Comp f1) as u1 ->
	  let newcont2 = function
	    | Local(Success(seq2,pt2)) -> cont(Fake(true,true,Comp(et f1 (fun cc -> cc (Local(Success(seq2,pt2)))) success seq)))
	    | Local(Fail _)            -> cont(Local(Fail seq))
	    | Fake(true,false,Comp f2) -> et f1 f2 success seq cont
	    | Fake(b1,b2,Comp f2)      -> cont(Fake(b1,b2,Comp(et (fun cc -> cc u1) f2 success seq)))
	  in
	  v2 newcont2
	| Fake(b1,b2,Comp f1)          -> cont(Fake(b1,b2,Comp(et f1 v2 success seq)))
      in
      v1 newcont1

     (* Unary version of ou and and, for homogeneous style *)

    let rec straight v success seq cont =
      let newcont = function
	| Local(Success(seqrec,pt))   -> cont(Local(success(seqrec,pt)))
	| Local(Fail _)               -> cont(Local(Fail seq))
	| Fake(b1,b2,Comp f)          -> cont(Fake(b1,b2,Comp(straight f success seq)))
       (* | Fake(b1,b2,Comp f)      -> cont (Fake(b1,b2,Comp f)) *)
      in
      v newcont


     (* Functions used to prune the generated proof-trees from useless formulae:
	relevant prunes sequent seq from the formulae not present in datastructure d
	update *)

    let rec ext = function
      | [] -> (ASet.empty,FSet.empty::FSet.empty::[])
      | p::l ->
	let (a,b) = Seq.interesting p in
	let (a',b') = ext l in
	(ASet.union a a', List.map2 FSet.union b b')

    let relevant (seq,d) = if !Flags.weakenings then
        match (seq,d) with
	| (Seq.EntF(_, g, _, _, polar),(atomN',formP'::formPSaved'::[])) ->
	  Seq.EntF(atomN', g, formP', formPSaved', polar)
	| (Seq.EntUF(_, delta, _, _, polar),(atomN',formP'::formPSaved'::[])) -> 
	  Seq.EntUF(atomN', delta, formP', formPSaved', polar)
	| (_,(_,l)) -> failwith("relevant - Wrong number of arguments: "^(string_of_int(List.length l)))
      else seq
        
    let success2 fseq = fun (seq1,pt1) (seq2,pt2) -> let seq = fseq seq1 seq2 in Success(seq,Proof.two seq pt1 pt2)
    let success1 fseq = fun (seqrec,pt)           -> let seq = fseq seqrec    in Success(seq,Proof.one seq pt)
    let successId     = fun (seq,pt)              -> Success(seq,pt)
    let success0 seq = Success(seq,Proof.zero seq)

    let std2 seq = let aux seq1 seq2 = relevant(seq,ext [seq1;seq2]) in success2 aux
    let std1 seq = let aux seqrec    = relevant(seq,ext [seqrec])    in success1 aux

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
      Dump.Kernel.incr_count 9;
      Dump.Kernel.print_time();
      if !Flags.debug>0 then Dump.msg None (Some("attack "^Seq.toString seq)) None;
      match seq with
      | Seq.EntF(atomN, g, formP, formPSaved, polar)
        -> begin match F.reveal g with
	| _ when ((polarity polar g) <> Pos) ->
	  straight
	    (lk_solve inloop (Seq.EntUF (atomN, FSet.add g FSet.empty, formP, formPSaved, polar)) data)
	    (std1 seq) seq cont

	| TrueP -> let x = success0(relevant(seq,(ASet.empty,FSet.empty::FSet.empty::[])))
	           in cont (Local(throw x))

	| FalseP -> cont (Local(throw(Fail seq)))
	  
	| AndP(a1, a2) ->
	  let u1 = lk_solve inloop (Seq.EntF (atomN, a1, formP, formPSaved, polar)) data in
	  let u2 = lk_solve inloop (Seq.EntF (atomN, a2, formP, formPSaved, polar)) data in
	  et u1 u2 (std2 seq) seq cont
	    
	| OrP(a1, a2) -> 
	  let side_pick b = 
            Dump.Kernel.fromPlugin();
	    let (a1',a2') = if b then (a1,a2) else (a2,a1) in
	    let u1 = lk_solve inloop (Seq.EntF (atomN, a1', formP, formPSaved, polar)) data in
	    let u2 = lk_solve inloop (Seq.EntF (atomN, a2', formP, formPSaved, polar)) data in
	    ou u1 u2 (std1 seq) (std1 seq) seq cont
	  in
          Dump.Kernel.toPlugin();
	  Fake(AskSide(seq,side_pick,data))

	| Lit t -> 
          Dump.Kernel.toTheory();
          let oracle = DecProc.goal_consistency atomN t in
          Dump.Kernel.fromTheory();
          let x = match oracle with
	  | None   -> Fail seq
	  | Some a -> success0(relevant(seq,(a,FSet.empty::FSet.empty::[])))
	           in cont (Local(throw x))

	| _ -> failwith("All cases should have been covered!")
	end
	
      | Seq.EntUF(atomN, delta, formP, formPSaved, polar) when not (FSet.is_empty delta)
	  -> let (toDecompose,newdelta) = FSet.next delta in
	     begin match F.reveal toDecompose with
	     | _ when (polarity polar toDecompose) = Pos 
                 ->if (FSet.is_in toDecompose formP)||(FSet.is_in toDecompose formPSaved)
	           then let u = lk_solve inloop (Seq.EntUF (atomN, newdelta, formP, formPSaved, polar)) data in
		        straight u (std1 seq) seq cont
	           else let u = lk_solve false (Seq.EntUF (atomN, newdelta, FSet.add toDecompose formP, formPSaved, polar)) data in
		        straight u (success1(fun pt->relevant(seq,
                                                              match ext [pt] with
		                                              | (ga,gfP::l) -> (ga,
				                                                (if FSet.is_in toDecompose gfP
				                                                 then FSet.remove toDecompose gfP
				                                                 else gfP)::l)
		                                              | _           -> failwith("Should not happen")))) seq cont

	     | TrueN -> let x = success0(relevant(seq,(ASet.empty,FSet.empty::FSet.empty::[])))
		        in cont (Local(throw x))

	     | FalseN -> let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar)) data in
		         straight u (std1 seq) seq cont

	     | Lit t when (polarity polar toDecompose) = Neg 
                     -> let t' = (MyTheory.Atom.negation t) in
		        if ASet.is_in t' atomN
		        then let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar)) data in
		             straight u (std1 seq) seq cont
		        else let u = lk_solve false (Seq.EntUF (ASet.add t' atomN,newdelta, formP, formPSaved, polar)) data in
		             straight u (success1(fun pt->relevant(seq,
							           let (ga,l) = ext [pt] in 
							           ((if ASet.is_in t' ga then ASet.remove t' ga else ga),l)))) seq cont

	     | Lit t when ((polarity polar toDecompose) = Und)
                 ->  (* print_string ("Hitting "^MyTheory.Atom.toString t^" in asynchronous phase\n"); *)
                   (* let newpolar = Pol.add t Neg (Pol.add (MyTheory.Atom.negation t) Pos polar) *)
                   let newpolar = Pol.add (MyTheory.Atom.negation t) Pos (Pol.add t Neg polar)
                   in
                   (* Pol.iter (fun l pol->print_endline(MyTheory.Atom.toString l^"->"^(match pol with Pos -> "Pos"| Neg->"Neg"|Und->"Und")))newpolar; *)
                   straight 
		     (lk_solve false (Seq.EntUF (atomN, delta, formP, formPSaved, newpolar)) data)
		     successId seq cont
                 
	     | AndN (a1, a2) -> 
	       let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 newdelta, formP, formPSaved, polar)) data in
	       let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a2 newdelta, formP, formPSaved, polar)) data in
	       et u1 u2 (std2 seq) seq cont
		 
	     | OrN (a1, a2) -> 
	       straight 
		 (lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 (FSet.add a2 newdelta), formP, formPSaved, polar)) data)
		 (std1 seq) seq cont

	     | _ -> failwith("All cases should have been covered!")
	     end

      | Seq.EntUF(atomN, _, formP', formPSaved', polar) 
	-> if !Flags.debug>0 then Dump.msg None (Some("attack "^Seq.toString seq)) None;

	  let rec lk_solvef formPChoose conschecked formP formPSaved action0 data cont = 

	    if ((FSet.is_empty formPChoose) && (FSet.is_empty formPSaved) && conschecked) 
	    then cont (Local(throw(Fail seq)))
	    else

	      let rec action_analysis =
                let intercept inter_fun v cont =
		  let newcont loc_ans = match inter_fun (prune loc_ans) with
		    | Action(f) -> Dump.Kernel.toPlugin();action_analysis f
		    | _         -> cont loc_ans
		  in v newcont
		in function instruction 
              ->
                Dump.Kernel.fromPlugin();	
                match instruction with
		| Focus(toFocus,inter_fun,l) 
                  ->Dump.Kernel.incr_count 4;(* real focus *)
		    if not (FSet.is_in toFocus formPChoose) then failwith("Not allowed to focus on this, you are cheating, you naughty!!!")
		    else
		      let u1 = lk_solve true  (Seq.EntF (atomN, toFocus, FSet.remove toFocus formP, FSet.add toFocus formPSaved, polar)) data in
		      let u2 = lk_solvef (FSet.remove toFocus formPChoose) conschecked formP formPSaved l data in
		      ou (intercept inter_fun u1) u2
			(success1(fun pt -> relevant(seq,
                                            match ext [pt] with
			                    | (ga,gfP::l) -> (ga,(FSet.add toFocus gfP)::l)
			                    | _           -> failwith("Should not happen"))))
			successId seq cont
			
		| Cut(3,toCut, inter_fun1, inter_fun2,l) (*cut_3*)
		  ->if !Flags.cuts = false then failwith("Cuts are not allowed");
		    Dump.Kernel.incr_count 5;
                    if !Flags.debug>0 then Dump.msg None (Some ("Cut3 on "^Form.toString toCut)) (Some 5);
                    let u1 = lk_solve true (Seq.EntF (atomN, toCut, formP, formPSaved, polar)) data in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar)) data in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l data in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq) u3 successId successId seq cont
                      
		| Cut(7,toCut, inter_fun1, inter_fun2,l) (*cut_7*)
		  ->if !Flags.cuts = false then failwith("Cuts are not allowed");
		    Dump.Kernel.incr_count 5;
                    if !Flags.debug>0 then Dump.msg None (Some ("Cut7 on "^Form.toString toCut)) (Some 5);
                    let u1 = lk_solve true (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, formPSaved, polar)) data in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar)) data in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l data in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq) u3 successId successId seq cont 
		(*	et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq cont *)

		| ConsistencyCheck(inter_fun,l) when not conschecked (*Checking consistency*)
		    ->let u1 cont =
                        Dump.Kernel.toTheory();
                        let oracle = DecProc.consistency atomN in
                        Dump.Kernel.fromTheory();
			let x = match oracle with
			  | None   -> Fail seq
			  | Some a -> success0(relevant(seq,(a,FSet.empty::FSet.empty::[])))
			in cont (Local(throw x))
		      in
		      let u2 = lk_solvef formPChoose true formP formPSaved l data in
		      ou u1 u2 successId (std1 seq) seq cont

		| Polarise(l, inter_fun) when (polarity polar (Form.lit l) = Und)
                    ->let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved,
							 Pol.add l Pos ( Pol.add (MyTheory.Atom.negation l) Neg polar))) data in
		      straight (intercept inter_fun u) successId seq cont
                        
		| DePolarise(l, inter_fun) when not (polarity polar (Form.lit l) = Und) 
                    ->if !Flags.depol = false then failwith("Depolarisation is not allowed");
		      let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved,
							 Pol.remove l ( Pol.remove (MyTheory.Atom.negation l) polar))) data in
		      straight (intercept inter_fun u) successId seq cont
                        
		| Propose(Fail s) when (Seq.subseq seq s)
                    ->cont (Local(throw (Fail s)))
                  
		| Propose(Success(s,pt)) when (Seq.subseq s seq)
                    ->cont (Local(throw (Success(s,pt))))
                  
		| Get(b1,b2,l)
                  -> cont (Fake(b1,!dir=b2,Comp(lk_solvef formPChoose conschecked formP formPSaved l data)))
                  
		| Restore l when not (FSet.is_empty formPSaved) 
                    ->if !Flags.fair && not (FSet.is_empty formPChoose)
		      then failwith("Trying to restore formulae on which focus has already been placed,
 but there still are formulae that you have not tried;
 your treatment is unfair");
		      lk_solvef (FSet.union formPChoose formPSaved) conschecked (FSet.union formP formPSaved) FSet.empty l data cont

		| _ -> failwith("focus_pick has suggested a stupid action")

	      in match action0() with
	      | Some(action)-> Dump.Kernel.toPlugin();action_analysis action
	      | None        -> (Dump.Kernel.toPlugin();
                                Fake(AskFocus(seq,formPChoose,not (FSet.is_empty formPSaved),conschecked,action_analysis,data)))
	  in
	  let newcont inter_fun loc_ans =
	    let recept_analysis _ = cont loc_ans in
	    recept_analysis (inter_fun (prune loc_ans))
	  in
	  let notify_analysis(accept_defeat,newdata,inter_fun,action1) =
            Dump.Kernel.fromPlugin();
	    if (inloop&&accept_defeat) then (Dump.Kernel.incr_count 7; cont (Local(throw(Fail seq))))
	    else lk_solvef formP' false formP' formPSaved' action1 newdata (newcont inter_fun)
	  in
	  Dump.Kernel.incr_count 8;
          Dump.Kernel.toPlugin();
	  Fake(Notify(seq,inloop,notify_analysis,data))

	
     (* Wraps the above function by providing top-level continuation
	inter (for interaction with user), and printing a couple of
	messages for standard output *)

    let rec wrap f =
      let fin w =
        Dump.Kernel.report w;
        Dump.Kernel.clear();
	dir := true
      in
      let inter = function
        | Fake(b1,b2,Comp v)
          -> dir:= not b2 ;
            let strg = "No more "
	      ^(if b1 then "Success" else "Failure")
	      ^" branch on the "
	      ^(if b2 then "right" else "left")
            in if !Flags.debug>0 then Dump.msg (Some strg) None None;
            Fake(Stop(b1,b2,fun _ -> wrap v))
        | Local(Success(s,p)) -> fin "Total Success"; Local(Success(s,p))
        | Local(Fail(s))      -> fin "Total Failure"; Local(Fail(s))
      in
      f inter

     (* Wraps the above function by providing initial inloop and
	initial sequent *)

    let machine seq data = Dump.Kernel.init(); wrap (lk_solve false seq data)

   end: sig
     module FE : (FrontEndType  with type litType     = MyTheory.Atom.t
				and  type formulaType = F.t
				and  type fsetType    = FSet.t
				and  type asetType    = ASet.t)
     val machine : FE.Seq.t -> 'a ->'a FE.output
   end)
