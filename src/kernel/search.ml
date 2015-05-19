open Interfaces_basic
open Interfaces_theory
open Formulae
open Interfaces_plugin
open Sequents

module ProofSearch 
  (MyTheory: DecProc)
  (PlDS: PlugDSType with type UASet.e = MyTheory.DS.IAtom.t
                    and  type UF.lit  = MyTheory.DS.Atom.t)
  =
  (struct

    open MyTheory
    open DS
    (* Loads the FrontEnd *)
    module FE = FrontEnd(DS)(PlDS)
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
      Dump.msg
        (Some (fun p -> p "%i %s: " (Dump.Kernel.read_count index) word))
        (Some (fun p -> p "%i %s: %a" (Dump.Kernel.read_count index) word
          (fun fmt -> function
          | Success(Genuine(s,_),_,_)-> Seq.print_in_fmt fmt s
          | Fail(Genuine s,_)        -> Seq.print_in_fmt fmt s
          | _ -> ())
          ans
         ))
        (Some index);
      ans

    let lift2local f = function
      | Genuine a -> Genuine (f a)
      | Fake b  -> Fake b

    (* Unary version of ou and et, for homogeneous style *)

    let rec straight v bfun chew sfun seq sigma cont = 
      let newcont = function
	| Success(ans,sigma',alt) -> 
          cont(Success((lift2local bfun) ans, sfun sigma', fun b -> straight (alt b) bfun chew sfun seq))
        | Fail(ans,f)             -> 
          cont(Fail((lift2local (fun _ -> seq)) ans, fun b -> straight (f b) bfun chew sfun seq))
      in
      v (chew sigma) newcont


    (* Combines two computations in OR style (with backtrack
       management): success = success for either computation *)

    let rec ou v1 v2 bfun1 bfun2 seq sigma cont =
      (* print_endline(Dump.toString(fun p-> p "ou on %a\nwith constraint %a" Seq.print_in_fmt seq Constraint.print_in_fmt sigma)); *)
      Dump.Kernel.incr_branches();
      let newcont1 u1 = Dump.Kernel.decr_branches(); match u1 with
	| Success(ans1,sigma1,alt1) ->
          cont(Success((lift2local bfun1) ans1, sigma1, fun b -> ou (alt1 b) v2 bfun1 bfun2 seq))

	| Fail(Fake false as ans1,f1)      ->
          cont(Fail(ans1,fun b -> ou (f1 b) v2 bfun1 bfun2 seq))

	| Fail(ans1,f1)             ->
	  v2 sigma (ouR (ans1,f1) bfun1 bfun2 seq sigma cont)
      in
      v1 sigma newcont1

    and ouR (ans1,f1) bfun1 bfun2 seq sigma cont u2 = 
      (* print_endline(Dump.toString(fun p-> p "ouR on %a\nwith constraint %a" Seq.print_in_fmt seq Constraint.print_in_fmt sigma)); *)
      let nextcont2 b = ouR (if b then (ans1,f1) else (Fake true,fun _ -> f1 false)) bfun1 bfun2 seq in
      match u2 with
      | Success(ans2,sigma2,alt2)-> 
        cont(Success((lift2local bfun2) ans2, sigma2, fun b' sigma' cc -> alt2 b' sigma' (nextcont2 b' sigma' cc)))
      | Fail(Genuine _ as ans2,f2) ->
        let newu2 b = if b then fun sigma' cc -> cc(Fail(ans2,f2)) else f2 false in
        cont(Fail(lift2local (fun _ -> seq) ans1,
                   match ans1 with
                   | Fake _   -> fun b' -> ou (f1 b') (newu2 b') bfun1 bfun2 seq
                   | Genuine _  -> fun b' sigma' cc -> newu2 b' sigma' (nextcont2 b' sigma' cc)
        ))
      | Fail(Fake b as ans2,f2) ->
        (match ans1 with
        | Fake _ when not b -> ou (f1 true) (f2 true) bfun1 bfun2 seq sigma cont
        | _                 -> cont(Fail(ans2,fun b' sigma' cc -> f2 b' sigma' (nextcont2 b' sigma' cc)))
        )

    (* Combines two computations in AND style (with backtrack
       management): success = success for both computations *)

    let rec et v1 v2 bfun seq sigma cont =  
      (* print_endline(Dump.toString(fun p-> p "et on %a\nwith constraint %a" Seq.print_in_fmt seq Constraint.print_in_fmt sigma)); *)
      Dump.Kernel.incr_branches();
      let newcont1 u1 = Dump.Kernel.decr_branches(); match u1 with
	| Fail(ans1,f1)   ->
          cont(Fail((lift2local (fun _ -> seq)) ans1, fun b -> et (f1 b) v2 bfun seq))

	| Success(Fake false as ans1,sigma1,alt1) ->
          cont(Success(ans1,sigma1,fun b -> et (alt1 b) v2 bfun seq))

	| Success(ans1,sigma1,alt1)       ->
	  let rec newcont2 (ans1,sigma1,alt1) cont u2 =
            let newf2 f2 b sigma cc = 
              if b then 
                match Constraint.meet sigma sigma1 with
                | None    -> et (alt1 true) (f2 false) bfun seq sigma cc 
                | Some sigma1' -> f2 true sigma1' (newcont2 (ans1,sigma1',alt1) cc)
              else
                f2 false sigma (newcont2 (Fake true,sigma,fun _ -> alt1 false) cc)
            in
            match u2 with
	    | Fail(Fake false as ans2,f2)
            | Fail(ans2,f2) when Constraint.compare sigma sigma1 = 0
                             -> cont(Fail(ans2,newf2 f2))
            | Fail(ans2,f2) -> et (f2 false) (alt1 true) (fun a2 a1 -> bfun a1 a2) seq sigma (ouR (ans2,newf2 f2) (fun a->a)(fun a->a) seq sigma cont)
	    | Success(ans2,sigma2,alt2) -> 
              (match ans1,ans2 with
              | Fake _, Fake false -> et (alt1 true) (alt2 true) bfun seq sigma2 cont
              | Fake _, Genuine a2   -> cont(Success(Fake true,sigma2,fun b -> et (alt1 b) (alt2 b) bfun seq))
              | Genuine a1, Genuine a2 -> cont(Success(Genuine (bfun a1 a2),sigma2,newf2 alt2))
              | _, _               -> cont(Success(ans2,sigma2,newf2 alt2))
              )
          in
          v2 sigma1 (newcont2 (ans1,sigma1,alt1) cont)
      in
      v1 sigma newcont1



    (* Functions used to prune the generated proof-trees from useless formulae:
       relevant prunes sequent seq from the formulae not present in datastructure d
       update *)

    let rec ext = function
      | []  -> failwith "Trying to use ext on empty list"
      | [p] -> Seq.interesting p
      | p::l ->
	let (a,b) = Seq.interesting p in
	let (a',b') = ext l in
	(ASet.union a a', List.map2 FSet.union b b')

    let relevant (seq,d) = if !Flags.weakenings then
        match (seq,d) with
	| (Seq.EntF(_, g, _, _, polar,ar),(atomN',formP'::formPSaved'::_)) ->
	  Seq.EntF(atomN', g, formP', formPSaved', polar,ar)
	| (Seq.EntUF(_, delta, _, _, polar,ar),(atomN',formP'::formPSaved'::delta'::_)) -> 
	  Seq.EntUF(atomN', FSet.inter delta delta', formP', formPSaved', polar,ar)
	| (_,(_,l)) -> failwith("relevant - Not enough arguments: "^(string_of_int(List.length l)))
      else seq
        
    let std0 seq  = Genuine(seq,Proof.zero seq)

    let add2delta form = function
      | (atomN',formP'::formPSaved'::delta'::[]) -> (atomN',formP'::formPSaved'::(FSet.add form delta')::[])
      | _ -> failwith "add2delta applied to structure with no delta"

    let std1 form seq (seqrec,pt) =
      match ext [seqrec] with
      | (_,_::_::gdelta::_) when FSet.is_empty gdelta -> (seqrec,pt)
      | l -> let newseq = 
               match form with
               | None   -> relevant(seq,l)
               | Some f -> relevant(seq,add2delta f l)
             in (newseq,Proof.one newseq pt)

    let std2 form seq (seq1,pt1) (seq2,pt2) =
      match ext [seq1], ext [seq2] with
      | (_,_::_::gdelta::_),_ when FSet.is_empty gdelta
          -> (seq1,pt1)
      | _,(_,_::_::gdelta::_) when FSet.is_empty gdelta
          -> (seq2,pt2)
      | _ -> let l = ext [seq1;seq2] in
             let newseq = 
               match form with
               | None   -> relevant(seq,l)
               | Some f -> relevant(seq,add2delta f l)
             in (newseq,Proof.two newseq pt1 pt2)

    let rec fail seq f = Fail(Genuine seq,failfunb seq f)
    and failfunb seq f b = if b then failfun seq f else f
    and failfun seq f _ cc = cc (fail seq f)

    let rec totalfailfun seq sigma cc = failfun seq (totalfailfun seq) sigma cc
    let totalfail seq = fail seq (totalfailfun seq)

    let prune = function
      | Success(Genuine(seq,pt),sigma,alt) -> Some(Provable(seq,pt,sigma))
      | Fail(Genuine seq,f) -> Some(NotProvable seq)
      | _                  -> None

    let bleft c = fun l -> c (true::l)
    let bright c = fun l -> c (false::l)

    (*
     * Main Search function 
     * delta = Formulae to be asynchronously decomposed 
     * atomN = negative atoms found in asynchronous phase (negation symbol not stored)
     * formP = positive formulae found in asynchronous phase (focus to be placed on them later)
     * formPTried = positive formulae found in asynchronous phase (focus on them has failed)
     * formPSaved = positive formulae on which focus has been placed "more times" than remaining formulae in formP 
     * (These other formulae have priority for focus -> ensures fairness)
     * Returns Provable(Prooftree) if a proof is found
     *)

    let rec lk_solve inloop seq data sigma cont =
      Dump.Kernel.incr_count 9;
      Dump.Kernel.print_time();
      Dump.msg None (Some(fun p -> p "---attack\n %a" Seq.print_in_fmt seq)) None;
      match seq with
      | Seq.EntF(atomN, ((g,tl) as ig), formP, formPSaved, polar,ar)
        -> begin match Formula.reveal g with
	| _ when ((Pol.form polar ig) <> Pos) ->
	  straight 
            (lk_solve inloop (Seq.EntUF (atomN, FSet.add ig FSet.empty, formP, formPSaved, polar,ar)) data)
            (std1 None seq) (fun a->a) (fun a->a) seq sigma cont
            
	| TrueP -> let x = Success(std0 (relevant(seq,(ASet.empty,FSet.empty::FSet.empty::[]))),
                                    sigma,
                                    failfunb seq (lk_solve inloop seq data))
	           in  cont (throw x)

	| FalseP -> cont (throw(totalfail seq))
	  
	| AndP(a1, a2) ->
	  let u1 = lk_solve inloop (Seq.EntF (atomN, (a1,tl), formP, formPSaved, polar,ar)) (bleft data) in
	  let u2 = lk_solve inloop (Seq.EntF (atomN, (a2,tl), formP, formPSaved, polar,ar)) (bright data) in
	  et u1 u2 (std2 None seq) seq sigma cont
	    
	| OrP(a1, a2) -> 
	  let side_pick (b,(newdata1,newdata2)) = 
            Dump.Kernel.fromPlugin();
	    let (a1',a2') = if b then (a1,a2) else (a2,a1) in
	    let u1 = lk_solve inloop (Seq.EntF (atomN, (a1',tl), formP, formPSaved, polar,ar)) newdata1 in
	    let u2 = lk_solve inloop (Seq.EntF (atomN, (a2',tl), formP, formPSaved, polar,ar)) newdata2 in
	    ou u1 u2 (std1 None seq) (std1 None seq) seq sigma cont
	  in
          Dump.Kernel.toPlugin();
	  InsertCoin(AskSide(seq,sigma,side_pick,data))

	| Exists(so,a) ->
          let (_,newar) as c = World.liftM so ar in
	  let u = lk_solve inloop (Seq.EntF (atomN, (a,DSubst.bind2FV c tl), formP, formPSaved, polar, newar)) data in
	  straight u (std1 None seq) (Constraint.liftM so) Constraint.projM seq sigma cont
            
	| Lit t -> 
          let rec pythie f sigma cont =
            Dump.Kernel.toTheory();
            let oracle = f sigma in
            Dump.Kernel.fromTheory();
            cont(throw(
              match oracle with
	      | NoMore             -> fail seq (pythie f)
	      | Guard(a,sigma',f') -> Success(std0(relevant(seq,(ASet.recons a,FSet.empty::FSet.empty::[]))),sigma',
                                               fun b -> if b then pythie f' else pythie f)
            ))
	  in pythie (goal_consistency (DS.iatom_build (t,tl)) (ASet.forTrusted atomN)) sigma cont

	| _ -> failwith "All cases should have been covered!"
	end
	
      | Seq.EntUF(atomN, delta, formP, formPSaved, polar,ar) when not (FSet.is_empty delta)
	  -> 
            let (paramformula,newdelta) = FSet.next delta in
             let (toDecompose,tl) = paramformula in
	     begin match Formula.reveal toDecompose with
	     | _ when (Pol.form polar paramformula) = Pos 
                 ->if (FSet.mem paramformula formP)||(FSet.mem paramformula formPSaved)
	           then let u = lk_solve inloop (Seq.EntUF (atomN, newdelta, formP, formPSaved, polar,ar)) data in
		        straight u (fun a->a) (fun a->a) (fun a->a) seq sigma cont
	           else let u = lk_solve false (Seq.EntUF (atomN, newdelta, FSet.add paramformula formP, formPSaved, polar,ar)) data in
		        straight u 
                          (fun (seqrec,pt) -> match ext [seqrec] with
		          | (ga,gfP::gfPS::gdelta::[]) when FSet.mem paramformula gfP
			      -> let newseq = relevant(seq,(ga,(FSet.remove paramformula gfP)::gfPS::(FSet.add paramformula gdelta)::[])) in
                                 (newseq,Proof.one newseq pt)
			  | _ -> (seqrec,pt))
                          (fun a->a)(fun a->a) seq sigma cont

	     | TrueN -> let x = Success(std0(relevant(seq,(ASet.empty,FSet.empty::FSet.empty::(FSet.add paramformula FSet.empty)::[]))),
                                         sigma,                                         
                                         failfunb seq (lk_solve inloop seq data))
		        in cont (throw x)

	     | FalseN -> let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar,ar)) data in
		         straight u (std1 (Some paramformula) seq) (fun a->a) (fun a->a) seq sigma cont

	     | Lit t when (Pol.form polar paramformula) = Neg 
                     ->  let t' = IAtom.negation(DS.iatom_build(t,tl)) in
		        if ASet.mem t' atomN
		        then let u = lk_solve inloop (Seq.EntUF (atomN,newdelta, formP, formPSaved, polar,ar)) data in
		             straight u (fun a->a)  (fun a->a) (fun a->a) seq sigma cont
		        else let u = lk_solve false (Seq.EntUF (ASet.add t' atomN,newdelta, formP, formPSaved, polar,ar)) data in
		             straight u 
                               (fun (seqrec,pt) -> match ext [seqrec] with
		               | (ga,gfP::gfPS::gdelta::[]) when ASet.mem t' ga
			           -> let newseq = relevant(seq,(ASet.remove t' ga, gfP::gfPS::(FSet.add paramformula gdelta)::[])) in
                                      (newseq,Proof.one newseq pt)
			       | _ -> (seqrec,pt))
                               (fun a->a) (fun a->a) seq sigma cont

	     | Lit t when (Pol.form polar paramformula) = Und
                     -> let newpolar = Pol.declarePos polar (IAtom.negation(DS.iatom_build(t,tl))) in
                        straight 
		          (lk_solve false (Seq.EntUF (atomN, delta, formP, formPSaved, newpolar,ar)) data)
		          (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                          
	     | AndN (a1, a2) -> 
	       let u1 = lk_solve inloop (Seq.EntUF (atomN, FSet.add (a1,tl) newdelta, formP, formPSaved, polar,ar)) (bleft data) in
	       let u2 = lk_solve inloop (Seq.EntUF (atomN, FSet.add (a2,tl) newdelta, formP, formPSaved, polar,ar)) (bright data) in
	       et u1 u2 (std2 (Some paramformula) seq) seq sigma cont
		 
	     | OrN (a1, a2) -> 
	       straight 
		 (lk_solve inloop (Seq.EntUF (atomN, FSet.add (a1,tl) (FSet.add (a2,tl) newdelta), formP, formPSaved, polar,ar)) data)
		 (std1 (Some paramformula) seq) (fun a->a)(fun a->a) seq sigma cont

	     | ForAll(so,a) ->
               let (_,newar) as c = World.liftE so ar in
	       let u = lk_solve inloop (Seq.EntUF (atomN, FSet.add (a,DSubst.bind2FV c tl) newdelta, formP, formPSaved, polar,newar)) data in
	       straight u (std1 (Some paramformula) seq) (Constraint.liftE so) Constraint.projE seq sigma cont

	     | _ -> failwith "All cases should have been covered!"

	     end

      | Seq.EntUF(atomN, _, formP', formPSaved', polar,ar)
	-> 

	  let rec lk_solvef formPChoose conschecked formP formPSaved action0 data sigma cont = 

	    if ((FSet.is_empty formPChoose) && (FSet.is_empty formPSaved) && conschecked) 
	    then cont (throw(totalfail seq))
	    else

	      let rec action_analysis =
                let intercept inter_fun v sigma cont =
		  let newcont loc_ans = (match prune loc_ans with Some a -> inter_fun a | None->()); cont loc_ans
		  in v sigma newcont
		in function instruction 
              ->
                Dump.Kernel.fromPlugin();	
                match instruction with
		| Focus(toFocus,(newdata1,newdata),inter_fun,l) 
                  ->Dump.Kernel.incr_count 4;(* real focus *)
		    if not (FSet.mem toFocus formPChoose) then raise (WrongInstructionException "Not allowed to focus on this, you are cheating, you naughty!!!")
		    else
		      let u1 = lk_solve true  (Seq.EntF (atomN, toFocus, FSet.remove toFocus formP, FSet.add toFocus formPSaved, polar,ar)) newdata1 in
		      let u2 = lk_solvef (FSet.remove toFocus formPChoose) conschecked formP formPSaved l newdata in
		      ou (intercept inter_fun u1) u2
                        (fun (seqrec,pt) -> match ext [seqrec] with
		        | (ga,gfP::gfPS::[])
			    -> let newseq = relevant(seq,(ga,(FSet.add toFocus gfP)::gfPS::FSet.empty::[])) in
                               (newseq,Proof.one newseq pt)
			| _ -> (seqrec,pt))
			(fun a->a) seq sigma cont
			
		| Cut(3,toCut,(newdata1,newdata), inter_fun1, inter_fun2,l) (*Focused Cut*)
		  ->if !Flags.cuts = false then raise (WrongInstructionException "Cuts are not allowed");
                    let (_,ds) = toCut in
                    if not (World.prefix (DSubst.get_arity ds) ar)
                    then raise (WrongInstructionException "The arity of cut formula is not a prefix of current arity");
		    Dump.Kernel.incr_count 5;
                    Dump.msg None (Some (fun p->p "Cut3 on %a" IForm.print_in_fmt toCut)) (Some 5);
                    let u1 = lk_solve true (Seq.EntF (atomN, toCut, formP, formPSaved, polar,ar)) (bleft newdata1) in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (IForm.negation toCut) FSet.empty, formP, formPSaved, polar,ar)) (bright newdata1) in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l newdata in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 None seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont
                      
		| Cut(7,toCut,(newdata1,newdata), inter_fun1, inter_fun2,l) (*Unfocused Cut*)
		  ->if !Flags.cuts = false then raise (WrongInstructionException "Cuts are not allowed");
                    let (_,ds) = toCut in
                    if not (World.prefix (DSubst.get_arity ds) ar)
                    then raise (WrongInstructionException "The arity of cut formula is not a prefix of current arity");
		    Dump.Kernel.incr_count 5;
                    let u1 = lk_solve true (Seq.EntUF (atomN, FSet.add toCut FSet.empty, formP, formPSaved, polar,ar)) (bleft newdata1) in
                    let u2 = lk_solve true (Seq.EntUF (atomN, FSet.add (IForm.negation toCut) FSet.empty, formP, formPSaved, polar,ar)) (bright newdata1) in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l newdata in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 None seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont

		| ACut(toCut,(newdata1,newdata), inter_fun1, inter_fun2,l) (*atomic_cut*)
		  ->if !Flags.cuts = false then raise (WrongInstructionException "Cuts are not allowed");
                    if not (DS.makes_sense toCut ar)
                    then raise (WrongInstructionException (Dump.toString(fun p-> p "Cut atom %a is not a prefix of current arity" IAtom.print_in_fmt toCut)));
                    let negtoCut = IAtom.negation toCut in
		    Dump.Kernel.incr_count 5;
                    let u1 = lk_solve 
                      (ASet.mem toCut atomN)
                      (Seq.EntUF (ASet.add toCut atomN,FSet.empty, formP, formPSaved, Pol.declarePos polar toCut,ar)) 
                      (bleft newdata1) in
                    let u2 = lk_solve 
                      (ASet.mem negtoCut atomN)
                      (Seq.EntUF (ASet.add negtoCut atomN, FSet.empty, formP, formPSaved, Pol.declarePos polar negtoCut,ar)) 
                      (bright newdata1) in
                    let u3 = lk_solvef formPChoose conschecked formP formPSaved l newdata in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 None seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont

		| ConsistencyCheck(newdata,inter_fun,l) when not conschecked (*Checking consistency*)
		    ->
                      let rec pythie f sigma cont =
                        Dump.Kernel.toTheory();
                        let oracle = f sigma in
                        Dump.Kernel.fromTheory();
                        cont(throw(
                          match oracle with
	                  | NoMore             -> fail seq (pythie f)
	                  | Guard(a,sigma',f') -> Success(std0(relevant(seq,(ASet.recons a,FSet.empty::FSet.empty::FSet.empty::[]))),sigma',
                                                           fun b -> if b then pythie f' else pythie f)
                        ))
                      in
                      let u2 = lk_solvef formPChoose true formP formPSaved l newdata in
                      ou (pythie (consistency (ASet.forTrusted atomN))) u2 (fun a->a) (fun a->a) seq sigma cont

		| Polarise(l,newdata, inter_fun) when (Pol.iatom polar l = Und)
                    ->let u = lk_solve false (Seq.EntUF(atomN, FSet.empty, formP, formPSaved, Pol.declarePos polar l, ar)) newdata in
		      straight (intercept inter_fun u) (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                        
		| DePolarise(l,newdata, inter_fun) when not (Pol.iatom polar l = Und) 
                    ->if !Flags.depol = false then raise (WrongInstructionException "Depolarisation is not allowed");
		      let u = lk_solve false (Seq.EntUF (atomN, FSet.empty, formP, formPSaved, Pol.remove polar l, ar)) newdata in
		      straight (intercept inter_fun u) (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                        
		| Propose(NotProvable s) when (Seq.subseq seq s)
                    ->cont (throw (fail s (lk_solvef formPChoose conschecked formP formPSaved (fun ()->None) data)))
                  
		| Propose(Provable(s,pt,sigma')) when (Seq.subseq s seq)
                    -> let resume = lk_solvef formPChoose conschecked formP formPSaved (fun ()->None) data
                       in
                       (match Constraint.meet sigma' sigma with
                       | None         -> straight resume (fun a->a) (fun a->a) (fun a->a) seq sigma cont
                       | Some sigma'' -> cont (throw (Success(Genuine(s,pt),sigma'', fun _ -> resume)))
                       )
                  
		| Get(b1,b2,l) when b1
                    -> cont (Success(Fake(!dir=b2),sigma,fun _ -> lk_solvef formPChoose conschecked formP formPSaved l data))

		| Get(b1,b2,l)
                  -> cont (Fail(Fake(!dir=b2),fun _ -> lk_solvef formPChoose conschecked formP formPSaved l data))
                  
		| Restore(newdata,inter_fun,l) when not (FSet.is_empty formPSaved) 
                    ->if !Flags.fair && not (FSet.is_empty formPChoose)
		      then raise (WrongInstructionException "Trying to restore formulae on which focus has already been placed,
 but there still are formulae that you have not tried;
 your treatment is unfair");
		      let u = lk_solvef (FSet.union formPChoose formPSaved) conschecked (FSet.union formP formPSaved) FSet.empty l newdata
                      in straight (intercept inter_fun u) (fun a->a) (fun a->a) (fun a->a) seq sigma cont

		| _ -> raise (WrongInstructionException "focus_pick has suggested a stupid action")

	      in match action0() with
	      | Some(action)-> Dump.Kernel.toPlugin();action_analysis action
	      | None        -> (Dump.Kernel.toPlugin();
                                InsertCoin(AskFocus(seq,sigma,formPChoose,not (FSet.is_empty formPSaved),conschecked,action_analysis,data)))
	  in
	  let newcont inter_fun loc_ans =
            (match prune loc_ans with Some a -> inter_fun a | None -> ()); cont loc_ans 
	  in
	  let notify_analysis(accept_defeat,newdata,inter_fun,action1) =
            Dump.Kernel.fromPlugin();
	    if (inloop&&accept_defeat) then (Dump.Kernel.incr_count 7; cont (throw(fail seq (lk_solve inloop seq data))))
	    else lk_solvef formP' false formP' formPSaved' action1 newdata sigma (newcont inter_fun)
	  in
	  Dump.Kernel.incr_count 8;
          Dump.Kernel.toPlugin();
	  InsertCoin(Notify(seq,sigma,inloop,notify_analysis,data))

	    
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
        | Success(Genuine(seq,pt),sigma,_) -> fin "PROVABLE"; Jackpot(Provable(seq,pt,sigma))
        | Fail(Genuine seq,_)              -> fin "NOT PROVABLE"; Jackpot(NotProvable seq)
        | Success(Fake b2,sigma,f) -> 
          dir:= not !dir ;
          let strg = if b2 then "right" else "left" in
          Dump.msg (Some (fun p->p "No more Success branch on the %s" strg)) None None;
          InsertCoin(Stop(true,b2,fun _ -> wrap (f true)))
        | Fail(Fake b2,f)          -> 
          dir:= not !dir ;
          let strg = if b2 then "right" else "left" in
          Dump.msg (Some (fun p->p "No more Failure branch on the %s" strg)) None None;
          InsertCoin(Stop(false,b2,fun _ -> wrap (f true)))
      in
      f Constraint.topconstraint inter

    (* Wraps the above function by providing initial inloop and
       initial sequent *)

    let machine_seq seq data = Dump.Kernel.init(); wrap (lk_solve false seq data)

    let machine formula init_data = 
      let seq = 
	Seq.EntUF(ASet.empty,FSet.add (formula,DSubst.init) FSet.empty, FSet.empty, FSet.empty,Pol.empty,World.init)
      in machine_seq seq (init_data seq)

   end: sig
     module FE : (FrontEndType  with type ASet.e      = PlDS.UASet.e
				and  type Form.lit    = PlDS.UF.lit
				and  type Form.datatype = PlDS.UF.t
				and  type FSet.ps     = PlDS.UFSet.t
				and  type ASet.ps     = PlDS.UASet.t)
     val machine : FE.Form.t -> (FE.Seq.t -> 'a FE.address) -> 'a FE.output
   end)
