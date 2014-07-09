open Formulae
open Interfaces
open Sequents

module ProofSearch 
  (MyTheory: DecProc)
  (F: FormulaImplem    with type lit = MyTheory.Atom.t)
  (FSet: CollectImplem with type e = F.t)
  (ASet: CollectImplem with type e = MyTheory.Atom.t)
  =
  (struct

     (* Loads the decision procedures *)
    module DecProc = MyTheory.Consistency(ASet)

     (* Loads the FrontEnd *)
    module FE = FrontEnd(MyTheory.Atom)(MyTheory.Constraint)(F)(FSet)(ASet)
    open FE
    

     (* Chooses whether, when faking failure, the natural
	behaviour is to go right (true) or left (false) *)

    let dir = ref true

     (* Throws a local answer ans on sequent s: printing message
	on standard output if debug mode is on, incrementing the
	adequate counter in the array *)

    let throw ans =
      let (index, word) = match ans with
	| ISuccess _ -> (0,"LocalSuccess")
	| IFail _    -> (1,"LocalFail")
      in
      Dump.Kernel.incr_count index;
      if !Flags.debug>0
      then (Dump.msg
              (Some (string_of_int (Dump.Kernel.read_count index)^" "^word^": "))
              (Some (string_of_int (Dump.Kernel.read_count index)^" "^word^": "
                     ^(match ans with
                       | ISuccess(Local(s,_),_,_)->Seq.toString s
                       | IFail(Local s,_)        ->Seq.toString s
                       | _ -> "")))
              (Some index));
      ans

    let lift2local f = function
      | Local a -> Local (f a)
      | Fake b  -> Fake b

    (* Unary version of ou and et, for homogeneous style *)

    let rec straight v bfun chew sfun seq sigma cont = 
      let newcont = function
	| ISuccess(ans,sigma',alt) -> 
          cont(ISuccess((lift2local bfun) ans, sfun sigma', straight alt bfun chew sfun seq))
	| IFail(ans,f)  -> 
          cont(IFail((lift2local (fun _->seq)) ans,straight f bfun chew sfun seq))
      in
      v (chew sigma) newcont



     (* Combines two computations in OR style (with backtrack
	management): success = success for either computation *)

    let rec ou v1 v2 bfun1 bfun2 seq sigma cont = Dump.Kernel.incr_branches();
      let newcont1 u1 = Dump.Kernel.decr_branches(); match u1 with
	| ISuccess(ans1,sigma1,alt1) ->
          cont(ISuccess((lift2local bfun1) ans1, sigma1, ou alt1 v2 bfun1 bfun2 seq))

	| IFail(Fake false,f1)       ->
          cont(IFail(Fake false,ou f1 v2 bfun1 bfun2 seq))

	| IFail(ans1,f1)                ->
	  let rec newcont2 cont = function
	    | ISuccess(ans2,sigma2,alt2)-> 
              cont(ISuccess((lift2local bfun2) ans2, sigma2, fun sigma cc -> alt2 sigma (newcont2 cc)))
	    | IFail(ans2,f2) ->
              let newalt sigma cc = f2 sigma (newcont2 cc) in
              (match ans1,ans2 with
              | Fake _, Fake false  -> ou f1 f2 bfun1 bfun2 seq sigma cont
              | Local _, Fake false -> cont(IFail(Fake false,newalt))
              | Local _, Local _    -> cont(IFail(Local seq,newalt))
              | _, _                -> cont(IFail(Fake true,newalt))
              )
	  in
	  v2 sigma (newcont2 cont)
      in
      v1 sigma newcont1

     (* Combines two computations in AND style (with backtrack
	management): success = success for both computations *)

    let rec et v1 v2 bfun seq sigma cont = Dump.Kernel.incr_branches();
      let newcont1 u1 = Dump.Kernel.decr_branches(); match u1 with
	| IFail(ans1,f1)   ->
          cont(IFail((lift2local (fun _ -> seq)) ans1, et f1 v2 bfun seq))

	| ISuccess(Fake false,sigma1,alt1) ->
          cont(ISuccess(Fake false,sigma1,et alt1 v2 bfun seq))

	| ISuccess(ans1,sigma1,alt1)        ->
	  let rec newcont2 cont = function
	    | IFail(Fake false,f2) -> cont(IFail(Fake false,fun sigma cc -> f2 sigma (newcont2 cc)))
	    | IFail(_,f2)          -> et f2 alt1 (fun a2 a1 -> bfun a1 a2) seq sigma cont
	    | ISuccess(ans2,sigma2,alt2) -> 
              let newalt sigma cc = alt2 sigma (newcont2 cc) in
              (match ans1,ans2 with
              | Fake  _, Fake false -> et alt1 alt2 bfun seq sigma2 cont
              | Local _, Fake false -> cont(ISuccess(Fake false,sigma2,newalt))
              | Local a1, Local a2  -> cont(ISuccess(Local (bfun a1 a2),sigma2,newalt))
              | _, _                -> cont(ISuccess(Fake true,sigma2,newalt))
              )
          in
          v2 sigma1 (newcont2 cont)
      in
      v1 sigma newcont1



     (* Functions used to prune the generated proof-trees from useless formulae:
	relevant prunes sequent seq from the formulae not present in datastructure d
	update *)

    let rec ext = function
      | []  -> failwith("Trying to use ext on empty list")
      | [p] -> Seq.interesting p
      | p::l ->
	let (a,b) = Seq.interesting p in
	let (a',b') = ext l in
	(ASet.union a a', List.map2 FSet.union b b')

    let relevant (seq,d) = if !Flags.weakenings then
        match (seq,d) with
	| (Seq.EntF(_, g, _, _, polar,ar),(atomN',formP'::formPSaved'::[])) ->
	  Seq.EntF(atomN', g, formP', formPSaved', polar,ar)
	| (Seq.EntUF(_, delta, _, _, polar,ar),(atomN',formP'::formPSaved'::delta'::[])) -> 
	  Seq.EntUF(atomN', FSet.inter delta delta', formP', formPSaved', polar,ar)
	| (_,(_,l)) -> failwith("relevant - Wrong number of arguments: "^(string_of_int(List.length l)))
      else seq
        
    let success2 fseq = fun (seq1,pt1) (seq2,pt2) -> let seq = fseq seq1 seq2 in (seq,Proof.two seq pt1 pt2)
    let success1 fseq = fun (seqrec,pt)           -> let seq = fseq seqrec    in (seq,Proof.one seq pt)
    let success0 seq  = Local(seq,Proof.zero seq)

    let std2 seq = let aux seq1 seq2 = relevant(seq,ext [seq1;seq2]) in success2 aux
    let std1 seq = let aux seqrec    = relevant(seq,ext [seqrec])    in success1 aux

    let add2delta form = function
      | (atomN',formP'::formPSaved'::delta'::[]) -> (atomN',formP'::formPSaved'::(FSet.add form delta')::[])
      | _ -> failwith("add2delta applied to structure with no delta")

    let stdU2 form seq = let aux seq1 seq2 = relevant(seq,add2delta form (ext [seq1;seq2])) in success2 aux
    let stdU1 form seq = let aux seqrec    = relevant(seq,add2delta form (ext [seqrec]))    in success1 aux

    let stdF1 seq = fun (seqrec,pt) -> 
      match ext [seqrec] with
      | (_,_::_::gdelta::[]) when FSet.is_empty gdelta
        -> (seqrec,pt)
      | (ga,gfP::gfPS::_)
          -> let newseq = relevant(seq,(ga,gfP::gfPS::[])) in
             (newseq,Proof.one newseq pt)
      | _ -> failwith("Not enough arguments in result of ext")

    let stdF2 seq = fun (seq1,pt1) (seq2,pt2) -> 
      match ext [seq1], ext [seq2] with
      | (_,_::_::gdelta::[]),_ when FSet.is_empty gdelta
          -> (seq1,pt1)
      | _,(_,_::_::gdelta::[]) when FSet.is_empty gdelta
          -> (seq2,pt2)
      | _ -> let newseq = relevant(seq,ext [seq1;seq2]) in
             (newseq,Proof.two newseq pt1 pt2)

    let prune = function
      | ISuccess(Local(seq,pt),sigma,alt) -> Some(Success(seq,pt,sigma))
      | IFail(Local seq,f) -> Some(Fail seq)
      | _                  -> None

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

    let rec lk_solve inloop seq data sigma cont =
      Dump.Kernel.incr_count 9;
      Dump.Kernel.print_time();
      if !Flags.debug>0 then Dump.msg None (Some("attack "^Seq.toString seq)) None;
      match seq with
      | Seq.EntF(atomN, g, formP, formPSaved, polar,ar)
        -> begin match F.reveal g with
	| _ when ((polarity polar g) <> Pos) ->
	  straight
	    (lk_solve inloop (Seq.EntUF (atomN, FSet.add g FSet.empty, formP, formPSaved, polar,ar)) data)
            (stdF1 seq)
            (fun a->a)
            (fun a->a)
            seq 
            sigma
            cont
            
	| TrueP -> let x = ISuccess(success0 (relevant(seq,(ASet.empty,FSet.empty::FSet.empty::[]))),
                                    sigma,
                                    lk_solve inloop seq data)
	           in  cont (throw x)

	| FalseP -> cont (throw(IFail(Local seq,lk_solve inloop seq data)))
	  
	| AndP(a1, a2) ->
	  let u1 = lk_solve inloop (Seq.EntF (atomN, a1, formP, formPSaved, polar,ar)) data in
	  let u2 = lk_solve inloop (Seq.EntF (atomN, a2, formP, formPSaved, polar,ar)) data in
	  et u1 u2 (stdF2 seq) seq sigma cont
	    
	| OrP(a1, a2) -> 
	  let side_pick b = 
            Dump.Kernel.fromPlugin();
	    let (a1',a2') = if b then (a1,a2) else (a2,a1) in
	    let u1 = lk_solve inloop (Seq.EntF (atomN, a1', formP, formPSaved, polar,ar)) data in
	    let u2 = lk_solve inloop (Seq.EntF (atomN, a2', formP, formPSaved, polar,ar)) data in
	    ou u1 u2 (stdF1 seq) (stdF1 seq) seq sigma cont
	  in
          Dump.Kernel.toPlugin();
	  Fake(AskSide(seq,sigma,side_pick,data))

	| Exists a ->
	  let u = lk_solve inloop (Seq.EntF (atomN, a, formP, formPSaved, polar, MyTheory.Constraint.addMeta ar)) data in
	  straight u (stdF1 seq) MyTheory.Constraint.lift MyTheory.Constraint.proj seq sigma cont
            
	| Lit t -> 
          let rec pythie f sigma cont =
            Dump.Kernel.toTheory();
            let oracle = f sigma in
            Dump.Kernel.fromTheory();
            cont(throw(
              match oracle with
	      | NoMore            -> IFail(Local seq,lk_solve inloop seq data)
	      | Guard(a,sigma',f') -> ISuccess(success0(relevant(seq,(a,FSet.empty::FSet.empty::[]))),sigma',pythie f')
            ))
	    in pythie (DecProc.goal_consistency atomN t) sigma cont

	| _ -> failwith("All cases should have been covered!")
	end
	
      | Seq.EntUF(atomN, delta, formP, formPSaved, polar,ar) when not (FSet.is_empty delta)
	  -> let (toDecompose,newdelta) = FSet.next delta in
	     begin match F.reveal toDecompose with
	     | _ when (polarity polar toDecompose) = Pos 
                 ->if (FSet.is_in toDecompose formP)||(FSet.is_in toDecompose formPSaved)
	           then let u = lk_solve inloop (Seq.EntUF (atomN, newdelta, formP, formPSaved, polar,ar)) data in
		        straight u (std1 seq) (fun a->a) (fun a->a) seq sigma cont
	           else let u = lk_solve false (Seq.EntUF (atomN, newdelta, FSet.add toDecompose formP, formPSaved, polar,ar)) data in
		        straight u (success1(fun pt->relevant(seq,
                                                              match ext [pt] with
		                                              | (ga,gfP::gfPS::gdelta::[]) when FSet.is_in toDecompose gfP
				                                  -> (ga,(FSet.remove toDecompose gfP)::gfPS::(FSet.add toDecompose gdelta)::[])
				                              | (ga,l) -> (ga,l)))) (fun a->a)(fun a->a) seq sigma cont

	     | TrueN -> let x = ISuccess(success0(relevant(seq,(ASet.empty,FSet.empty::FSet.empty::(FSet.add toDecompose FSet.empty)::[]))),
                                         sigma,                                         
                                         lk_solve inloop seq data)
		        in cont (throw x)

	     | FalseN -> let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar,ar)) data in
		         straight u (std1 seq) (fun a->a) (fun a->a) seq sigma cont

	     | Lit t when (polarity polar toDecompose) = Neg 
                     -> let t' = (MyTheory.Atom.negation t) in
		        if ASet.is_in t' atomN
		        then let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar,ar)) data in
		             straight u (std1 seq)  (fun a->a) (fun a->a) seq sigma cont
		        else let u = lk_solve false (Seq.EntUF (ASet.add t' atomN,newdelta, formP, formPSaved, polar,ar)) data in
		             straight u (success1(fun pt->relevant(seq,
                                                              match ext [pt] with
		                                              | (ga,gfP::gfPS::gdelta::[]) when ASet.is_in t' ga
				                                  -> (ASet.remove t' ga, gfP::gfPS::(FSet.add toDecompose gdelta)::[])
				                              | (ga,l) -> (ga,l))))  (fun a->a) (fun a->a) seq sigma cont

	     | Lit t when ((polarity polar toDecompose) = Und)
                 ->  (* print_string ("Hitting "^MyTheory.Atom.toString t^" in asynchronous phase\n"); *)
                   (* let newpolar = Pol.add t Neg (Pol.add (MyTheory.Atom.negation t) Pos polar) *)
                   let newpolar = Pol.add (MyTheory.Atom.negation t) Pos (Pol.add t Neg polar)
                   in
                   (* Pol.iter (fun l pol->print_endline(MyTheory.Atom.toString l^"->"^(match pol with Pos -> "Pos"| Neg->"Neg"|Und->"Und")))newpolar; *)
                   straight 
		     (lk_solve false (Seq.EntUF (atomN, delta, formP, formPSaved, newpolar,ar)) data)
		     (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                 
	     | AndN (a1, a2) -> 
	       let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 newdelta, formP, formPSaved, polar,ar)) data in
	       let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add a2 newdelta, formP, formPSaved, polar,ar)) data in
	       et u1 u2 (stdU2 toDecompose seq) seq sigma cont
		 
	     | OrN (a1, a2) -> 
	       straight 
		 (lk_solve inloop (Seq.EntUF (atomN, FSet.add a1 (FSet.add a2 newdelta), formP, formPSaved, polar,ar)) data)
		 (stdU1 toDecompose seq) (fun a->a)(fun a->a) seq sigma cont

	     | ForAll a ->
	       let u = lk_solve inloop (Seq.EntUF (atomN, FSet.add a newdelta, formP, formPSaved, polar,MyTheory.Constraint.addEigen ar)) data in
	       straight u (stdF1 seq) (fun sigma->sigma) (fun sigma->sigma) seq sigma cont

	     | _ -> failwith("All cases should have been covered!")

	     end

      | Seq.EntUF(atomN, _, formP', formPSaved', polar,ar) 
	-> if !Flags.debug>0 then Dump.msg None (Some("attack "^Seq.toString seq)) None;

	  let rec lk_solvef formPChoose conschecked formP formPSaved action0 data sigma cont = 

	    if ((FSet.is_empty formPChoose) && (FSet.is_empty formPSaved) && conschecked) 
	    then cont (throw(IFail(Local seq,lk_solvef formPChoose conschecked formP formPSaved action0 data)))
	    else

	      let rec action_analysis =
                let intercept inter_fun v sigma cont =
		  let newcont loc_ans = (match prune loc_ans with Some a -> inter_fun a | None->()); cont loc_ans
		  in v sigma newcont
		in function instruction 
              ->
                Dump.Kernel.fromPlugin();	
                match instruction with
		| Focus(toFocus,inter_fun,l) 
                  ->Dump.Kernel.incr_count 4;(* real focus *)
		    if not (FSet.is_in toFocus formPChoose) then failwith("Not allowed to focus on this, you are cheating, you naughty!!!")
		    else
		      let u1 = lk_solve true  (Seq.EntF (atomN, toFocus, FSet.remove toFocus formP, FSet.add toFocus formPSaved, polar,ar)) data in
		      let u2 = lk_solvef (FSet.remove toFocus formPChoose) conschecked formP formPSaved l data in
		      ou (intercept inter_fun u1) u2
			(success1(fun pt -> relevant(seq,
                                            match ext [pt] with
			                    | (ga,gfP::gfPS::[]) -> (ga,(FSet.add toFocus gfP)::gfPS::FSet.empty::[])
			                    | (ga,l)             -> (ga,l))))
			(fun a->a) seq sigma cont
			
		| Cut(3,toCut, inter_fun1, inter_fun2,l) (*cut_3*)
		  ->if !Flags.cuts = false then failwith("Cuts are not allowed");
		    Dump.Kernel.incr_count 5;
                    if !Flags.debug>0 then Dump.msg None (Some ("Cut3 on "^Form.toString toCut)) (Some 5);
                    let u1 = lk_solve true (Seq.EntF (atomN, toCut, formP, formPSaved, polar,ar)) data in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar,ar)) data in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l data in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont
                      
		| Cut(7,toCut, inter_fun1, inter_fun2,l) (*cut_7*)
		  ->if !Flags.cuts = false then failwith("Cuts are not allowed");
		    Dump.Kernel.incr_count 5;
                    if !Flags.debug>0 then Dump.msg None (Some ("Cut7 on "^Form.toString toCut)) (Some 5);
                    let u1 = lk_solve true (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, formPSaved, polar,ar)) data in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (Form.negation toCut) FSet.empty, formP, formPSaved, polar,ar)) data in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l data in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont 
		(*	et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 seq) seq cont *)

		| ConsistencyCheck(inter_fun,l) when not conschecked (*Checking consistency*)
		    ->let rec pythie f sigma cont =
                        Dump.Kernel.toTheory();
                        let oracle = f sigma in
                        Dump.Kernel.fromTheory();
                        cont(throw(
                          match oracle with
	                  | NoMore            -> IFail(Local seq,lk_solve inloop seq data)
	                  | Guard(a,sigma',f') -> ISuccess(success0(relevant(seq,(a,FSet.empty::FSet.empty::[]))),sigma',pythie f')
                        ))
                      in
                      let u2 = lk_solvef formPChoose true formP formPSaved l data in
                      ou (pythie (DecProc.consistency atomN)) u2 (fun a->a) (fun a->a) seq sigma cont

		| Polarise(l, inter_fun) when (polarity polar (Form.lit l) = Und)
                    ->let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved,
							 Pol.add l Pos ( Pol.add (MyTheory.Atom.negation l) Neg polar),
                                                         ar)) data in
		      straight (intercept inter_fun u) (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                        
		| DePolarise(l, inter_fun) when not (polarity polar (Form.lit l) = Und) 
                    ->if !Flags.depol = false then failwith("Depolarisation is not allowed");
		      let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved,
							 Pol.remove l (Pol.remove (MyTheory.Atom.negation l) polar),
                                                         ar)) data in
		      straight (intercept inter_fun u) (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                        
		| Propose(Fail s) when (Seq.subseq seq s)
                    ->cont (throw (IFail(Local s,lk_solvef formPChoose conschecked formP formPSaved action0 data)))
                  
		| Propose(Success(s,pt,sigma')) when (Seq.subseq s seq)
                    ->cont (throw (ISuccess(Local(s,pt),sigma',lk_solvef formPChoose conschecked formP formPSaved action0 data)))
                  
		| Get(b1,b2,l) when b1
                  -> cont (ISuccess(Fake(!dir=b2),sigma,lk_solvef formPChoose conschecked formP formPSaved l data))

		| Get(b1,b2,l)
                  -> cont (IFail(Fake(!dir=b2),lk_solvef formPChoose conschecked formP formPSaved l data))
                  
		| Restore l when not (FSet.is_empty formPSaved) 
                    ->if !Flags.fair && not (FSet.is_empty formPChoose)
		      then failwith("Trying to restore formulae on which focus has already been placed,
 but there still are formulae that you have not tried;
 your treatment is unfair");
		      lk_solvef (FSet.union formPChoose formPSaved) conschecked (FSet.union formP formPSaved) FSet.empty l data sigma cont

		| _ -> failwith("focus_pick has suggested a stupid action")

	      in match action0() with
	      | Some(action)-> Dump.Kernel.toPlugin();action_analysis action
	      | None        -> (Dump.Kernel.toPlugin();
                                Fake(AskFocus(seq,sigma,formPChoose,not (FSet.is_empty formPSaved),conschecked,action_analysis,data)))
	  in
	  let newcont inter_fun loc_ans =
            (match prune loc_ans with Some a -> inter_fun a | None -> ()); cont loc_ans 
	  in
	  let notify_analysis(accept_defeat,newdata,inter_fun,action1) =
            Dump.Kernel.fromPlugin();
	    if (inloop&&accept_defeat) then (Dump.Kernel.incr_count 7; cont (throw(IFail(Local seq,lk_solve inloop seq data))))
	    else lk_solvef formP' false formP' formPSaved' action1 newdata sigma (newcont inter_fun)
	  in
	  Dump.Kernel.incr_count 8;
          Dump.Kernel.toPlugin();
	  Fake(Notify(seq,sigma,inloop,notify_analysis,data))

	
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
        | ISuccess(Local(seq,pt),sigma,f) -> fin "Total Success"; Local(Success(seq,pt,sigma))
        | IFail(Local seq,f)              -> fin "Total Failure"; Local(Fail seq)
        | ISuccess(Fake b2,sigma,f) -> 
          dir:= not !dir ;
          let strg = "No more Success branch on the "^(if b2 then "right" else "left")
          in if !Flags.debug>0 then Dump.msg (Some strg) None None;
          Fake(Stop(true,b2,fun _ -> wrap f))
        | IFail(Fake b2,f)          -> 
          dir:= not !dir ;
          let strg = "No more Failure branch on the "^(if b2 then "right" else "left")
          in if !Flags.debug>0 then Dump.msg (Some strg) None None;
          Fake(Stop(false,b2,fun _ -> wrap f))
      in
      f MyTheory.Constraint.topconstraint inter

     (* Wraps the above function by providing initial inloop and
	initial sequent *)

    let machine seq data = Dump.Kernel.init(); wrap (lk_solve false seq data)

   end: sig
     module FE : (FrontEndType  with type litType     = MyTheory.Atom.t
				and  type constraints = MyTheory.Constraint.t
				and  type arities     = MyTheory.Constraint.arities
				and  type formulaType = F.t
				and  type fsetType    = FSet.t
				and  type asetType    = ASet.t)
     val machine : FE.Seq.t -> 'a ->'a FE.output
   end)
