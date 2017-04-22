open Top
open Interfaces_basic
open Variables
open Termstructures.Literals
open Formulae
open APIplugin
    
module ProofSearch(PlDS: PlugDSType) = struct

  include Sequents.Make(PlDS)

  module Make(MyTheory: Specs.DSproj with type ts = PlDS.UF.t FormulaF.generic) = struct

    open MyTheory
    (* Loads the FrontEnd *)
    module FE = FrontEnd(MyTheory)
    module Constraint = FirstOrder.Constraint
    open FE

    (* type final =  *)
    (* | L of (sign,ASet.t,thProvable) thsays *)
    (* | R of (sign,ASet.t,thNotProvable) thsays *)

    (* let mygoal_consistency t atomN = *)
    (*   if ASet.mem t atomN *)
    (*   then L(thProvable () (ASet.add t ASet.empty)) *)
    (*   else R(thNotProvable () atomN) *)

    (* let myconsistency atomN = ASet.fold *)
    (*   (function l -> function *)
    (*   | L(ThProvable set) as ans -> ans *)
    (*   | _ -> (match mygoal_consistency (LitF.negation l) atomN with *)
    (*     | L(ThProvable set) -> L(thProvable () (ASet.add l set)) *)
    (*     | ans -> ans )) *)
    (*   atomN *)
    (*   (R(thNotProvable () atomN)) *)

    
    (* Chooses whether, when faking failure, the natural
       behaviour is to go right (true) or left (false) *)

    let dir = ref true

    (* Throws a local answer ans on sequent s: printing message
       on standard output if debug mode is on, incrementing the
       adequate counter in the array *)

    let throw ans =
      let index, word = match ans with
	| Success _ -> (0,"LocalSuccess")
	| Fail _    -> (1,"LocalFail")
      in
      Dump.Kernel.incr_count index;
      (* Dump.print ["prop_search",1]  *)
      (*   (fun p -> p "%i %s: %a" (Dump.Kernel.read_count index) word *)
      (*     (fun fmt -> function *)
      (*     | Success(Genuine(s,_),_,_)-> Seq.print_in_fmt fmt s *)
      (*     | Fail(Genuine s,_)        -> Seq.print_in_fmt fmt s *)
      (*     | _ -> ()) *)
      (*     ans *)
      (*    ); *)
      ans

    let lift2local f = function
      | Genuine a -> Genuine (f a)
      | Fake b  -> Fake b

    (* Unary version of ou and et, for homogeneous style *)
    type 'b continuation
      = 'b output intern -> 'b output

    type 'b job = Constraint.t -> 'b continuation -> 'b output
                                        
    let rec straight : type a.
                            ((Seq.t*Proof.t) -> (Seq.t*Proof.t))
                            -> ?inject:(Constraint.t -> Constraint.t)
                            -> ?project:(Constraint.t -> Constraint.t)
                            -> a seq
                            -> 'b job
                            -> 'b job =
      fun bfun ?(inject=(fun a->a)) ?(project=(fun a->a)) seq v sigma cont ->
      let newcont = function
	| Success(ans,sigma',alt) -> 
           cont(Success(lift2local bfun ans, project sigma', fun b -> straight bfun ~inject ~project seq (alt b)))
        | Fail(ans,f)             -> 
           cont(Fail(lift2local (fun _ -> Seq.Seq seq) ans, fun b -> straight bfun ~inject ~project seq (f b)))
      in
      v (inject sigma) newcont


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
        cont(Fail(lift2local (fun _ -> Seq.Seq seq) ans1,
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
          cont(Fail((lift2local (fun _ -> Seq.Seq seq)) ans1, fun b -> et (f1 b) v2 bfun seq))

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
      | [Seq.Seq p] -> Seq.interesting p
      | (Seq.Seq p)::l ->
	let (a,b) = Seq.interesting p in
	let (a',b') = ext l in
	(ASet.union a a', List.map2 FSet.union b b')

    let relevant (type a) ((seq : a seq), d) = if !Flags.weakenings then
        match seq.rhs, d with
	| F _,     (atomN',formP'::formPSaved'::_) ->
	  { seq with lits=atomN'; formP=formP'; formPSaved=formPSaved'}
	| U delta, (atomN',formP'::formPSaved'::delta'::_) -> 
	  { seq with lits=atomN'; rhs = U(FSet.inter delta delta'); formP=formP'; formPSaved=formPSaved'}
	| (_,(_,l)) -> failwith("relevant - Not enough arguments: "^(string_of_int(List.length l)))
      else seq
        
    let std0 seq  = Genuine(Seq.Seq seq,Proof.zero (Seq.Seq seq))

    let add2delta form = function
      | (atomN',formP'::formPSaved'::delta'::[]) -> (atomN',formP'::formPSaved'::(FSet.add form delta')::[])
      | _ -> failwith "add2delta applied to structure with no delta"

    let std1 ?form seq (seqrec,pt) =
    (*   let l = ext [seqrec] in *)
    (*   let newseq = match form with *)
    (*     | None   -> relevant(seq,l) *)
    (*     | Some f -> relevant(seq,add2delta f l) *)
    (*   in *)
    (*   (Seq.Seq newseq,Proof.one (Seq.Seq newseq) pt) *)


      (* let std1 form seq (seqrec,pt) = *)
      match ext [seqrec] with
      | (_,_::_::gdelta::_) when FSet.is_empty gdelta -> (seqrec,pt)
      | l -> let newseq =
               Seq.Seq 
                 (match form with
                  | None   -> relevant(seq,l)
                  | Some f -> relevant(seq,add2delta f l))
             in (newseq,Proof.one newseq pt)

                  
    let std2 form (type a) (seq: a seq) (seq1,pt1) (seq2,pt2) =
      match ext [seq1], ext [seq2] with
      | (_,_::_::gdelta::_),_ when FSet.is_empty gdelta
        -> (seq1,pt1)
      | _,(_,_::_::gdelta::_) when FSet.is_empty gdelta
        -> (seq2,pt2)
      | _ -> let l = ext [seq1;seq2] in
             let newseq = match form with
               | None   -> relevant(seq,l)
               | Some f -> relevant(seq,add2delta f l)
             in
             (Seq.Seq newseq,Proof.two (Seq.Seq newseq) pt1 pt2)

    let rec fail seq f = Fail(Genuine(Seq.Seq seq),failfunb seq f)
    and failfunb seq f b = if b then failfun seq f else f
    and failfun seq f _ cc = cc (fail seq f)

    let rec totalfailfun seq sigma cc = failfun seq (totalfailfun seq) sigma cc
    let totalfail seq = fail seq (totalfailfun seq)

    let prune : 'a FE.output FE.intern -> seqU answer option = function
      | Success(Genuine(Seq.Seq seq,pt),sigma,alt) ->
         begin
           match seq.rhs with
           | U _ -> Some(Provable(seq,pt,sigma))
           | F _ -> None
         end
      | Fail(Genuine(Seq.Seq seq),f) -> 
         begin
           match seq.rhs with
           | U _ -> Some(NotProvable seq)
           | F _ -> None
         end
      | _                  -> None

    let bleft c l = c (true::l)
    let bright c l = c (false::l)

    (*
     * Main Search function 
     * Returns Provable(Prooftree) if a proof is found
     *)

    type ('a,'b) state = {
        inloop : bool;
        seq    : 'a seq;
        data   : 'b address
      }

    type 'b statef = {
        seqf     : seqU seq;
        dataf    : 'b FE.address;
        toChoose : FSet.t;
        conschecked : bool;
        nextaction  : 'b alt_action
      }

    let rec lk_solve : type a b. (a,b) state -> b job
      = fun state sigma cont ->
      Dump.Kernel.incr_count 9;
      Dump.Kernel.print_time();
      let seq = state.seq in
      Dump.print ["prop_search",2] (fun p -> p "---attack\n %a" Seq.print_seq_in_fmt seq);
      match seq.rhs with
      | F g
      (* | Seq.EntF(atomN, g, formP, formPSaved, polar,ar) *)
        -> begin match FormulaF.reveal g with
	| _ when not(is_Pos(Pol.form seq.polar g)) ->
	   straight (std1 seq) seq
             (lk_solve { state with seq = { seq with rhs = U(FSet.singleton g)} } )
             sigma cont
            
	| TrueP -> 
           let x = Success(std0 (relevant(seq,(ASet.empty,FSet.empty::FSet.empty::[]))),
                           sigma,
                           failfunb seq (fun sigma cont->lk_solve state sigma cont)) in
           cont (throw x)

	| FalseP -> cont (throw(totalfail seq))
	  
	| AndP(a1, a2) ->
	   let u1 = lk_solve { state with data = bleft state.data;
                                          seq = { seq with rhs = F a1 } } in
	   let u2 = lk_solve { state with data = bright state.data;
                                          seq = { seq with rhs = F a2} } in
	   et u1 u2 (std2 None seq) seq sigma cont
	    
	| OrP(a1, a2) -> 
	  let side_pick (b,(newdata1,newdata2)) = 
            Dump.Kernel.fromPlugin();
	    let a1', a2' = if b then (a1,a2) else (a2,a1) in
	    let u1 = lk_solve { state with data = newdata1;
                                           seq  = { seq with rhs = F a1' }} in
	    let u2 = lk_solve { state with data = newdata2;
                                           seq  = { seq with rhs = F a2' }} in
	    ou u1 u2 (std1 seq) (std1 seq) seq sigma cont
	  in
          Dump.Kernel.toPlugin();
	  InsertCoin(AskSide(seq,sigma,side_pick,state.data))

	| ExistsF(so,a,tl) ->
          let (_,newworld) as c = World.liftM so seq.world in
          let d = c::tl in
	  let u = lk_solve { state with data = state.data;
                                        seq = { seq with rhs = F(proj(Terms.data(Term.lift d a)));
                                                         world = newworld }} in
	  straight (std1 seq) ~inject:(Constraint.lift newworld) ~project:Constraint.proj seq u sigma cont
            
	| LitF t -> 
          let rec pythie f sigma cont =
            Dump.Kernel.toTheory();
            let oracle = f sigma in
            Dump.Kernel.fromTheory();
            cont(throw(
              match oracle with
	      | NoMore             -> fail seq (pythie f)
	      | Guard(a,sigma',f') -> Success(std0(relevant(seq,(asASet a,FSet.empty::FSet.empty::[]))),sigma',
                                              fun b -> if b then pythie f' else pythie f)
            ))
	  in
          InsertCoin(CloseNow(litF_as_term t,asAssign seq.lits, fun f -> pythie f sigma cont))
          (* pythie (MyTheory.goal_consistency () ()) sigma cont *)
        (* pythie (fun _ -> mygoal_consistency t atomN) sigma cont *)

	| _ -> failwith "All cases should have been covered!"
	   end
	     
      | U delta when not (FSet.is_empty delta) ->
         
         let toDecompose, newdelta = FSet.next delta in
	 begin match FormulaF.reveal toDecompose with

	 | _ when is_Pos (Pol.form seq.polar toDecompose) ->

            if (FSet.mem toDecompose seq.formP)||(FSet.mem toDecompose seq.formPSaved)
	    then let u = lk_solve { state with seq = { seq with rhs = U newdelta }} in
		 straight (fun a->a) seq u sigma cont
	    else let u = lk_solve { state with inloop = false;
                                               seq = {seq with rhs = U newdelta;
                                                               formP = FSet.add toDecompose seq.formP }} in
		 straight 
                   (fun (seqrec,pt) ->
                     match ext [seqrec] with
		     | (ga,gfP::gfPS::gdelta::[]) when FSet.mem toDecompose gfP
		       -> let newseq = relevant(seq,(ga,(FSet.remove toDecompose gfP)::gfPS::(FSet.add toDecompose gdelta)::[])) in
                          (Seq.Seq newseq,Proof.one (Seq.Seq newseq) pt)
		     | _ -> (seqrec,pt))
                   seq u sigma cont

	 | TrueN ->
            let x = Success(std0(relevant(seq,(ASet.empty,FSet.empty::FSet.empty::(FSet.add toDecompose FSet.empty)::[]))),
                            sigma,                                         
                            failfunb seq (lk_solve state))
	    in cont (throw x)
                    
	 | FalseN ->
            let u = lk_solve { state with seq = { seq with rhs = U newdelta }} in
	    straight (std1 ~form:toDecompose seq) seq u sigma cont

	 | LitF t when is_Neg (Pol.form seq.polar toDecompose) ->
            let t' = LitF.negation t in
	    if ASet.mem t' seq.lits
	    then let u = lk_solve { state with seq = { seq with rhs = U newdelta }} in
		 straight (fun a->a) seq u sigma cont
	    else let u = lk_solve { state with inloop = false;
                                               seq = {seq with lits = ASet.add t' seq.lits;
                                                               rhs = U newdelta } } in
		 straight  
                   (fun (seqrec,pt) ->
                     match ext [seqrec] with
		     | (ga,gfP::gfPS::gdelta::[]) when ASet.mem t' ga
		       -> let newseq = relevant(seq,(ASet.remove t' ga, gfP::gfPS::(FSet.add toDecompose gdelta)::[])) in
                          (Seq.Seq newseq,Proof.one (Seq.Seq newseq) pt)
		     | _ -> (seqrec,pt))
                   seq u sigma cont

	 | LitF t when is_Und(Pol.form seq.polar toDecompose) ->
            let newpolar = Pol.declarePos seq.polar (LitF.negation t) in
            straight (fun a->a) seq
              (lk_solve { state with inloop = false;
                                     seq = { seq with polar = newpolar } })
              sigma cont
              
	 | AndN (a1, a2) -> 
	    let u1 = lk_solve { state with data = bleft state.data;
                                           seq = { seq with rhs = U(FSet.add a1 newdelta) }} in
	    let u2 = lk_solve { state with data = bright state.data;
                                           seq = { seq with rhs = U(FSet.add a2 newdelta) }} in
	    et u1 u2 (std2 (Some toDecompose) seq) seq sigma cont
	       
	 | OrN (a1, a2) -> 
	    straight 
	      (std1 ~form:toDecompose seq)
              seq
              (lk_solve { state with seq = { seq with rhs = U(FSet.add a1 (FSet.add a2 newdelta)) }} )
              sigma cont

	 | ForAllF(so,a,tl) ->
            let (_,newworld) as c = World.liftE so seq.world in
            let d = c::tl in
	    let u = lk_solve { state with seq = { seq with rhs = U(FSet.add (proj(Terms.data(Term.lift d a))) newdelta);
                                                           world = newworld }} in
	    straight (std1 ~form:toDecompose seq)
              ~inject:(Constraint.lift newworld)
              ~project:Constraint.proj
              seq u sigma cont

	 | _ -> failwith "All cases should have been covered!"

	 end

      | U _ ->

	 let rec lk_solvef state (* formPChoose conschecked formP formPSaved action0 data *) sigma cont = 
           Dump.print ["prop_search",2] (fun p -> p "---attack lk_solvef\n %a" Seq.print_seq_in_fmt seq);

           let seq = state.seqf in

	   if ((FSet.is_empty state.toChoose) && (FSet.is_empty seq.formPSaved) && state.conschecked) 
	   then cont (throw(totalfail seq))
	   else

	     let rec action_analysis =
               let intercept inter_fun v sigma cont =
		 let newcont loc_ans = (match prune loc_ans with Some a -> inter_fun a | None->()); cont loc_ans
		 in v sigma newcont
	       in function
                 instruction ->
                 Dump.Kernel.fromPlugin();	
                 match instruction with
	         | Focus(toFocus,(newdata1,newdata),inter_fun,l) ->
                    Dump.Kernel.incr_count 4;(* real focus *)
		    if not (FSet.mem toFocus state.toChoose)
                    then raise (WrongInstructionException "Not allowed to focus on this, you are cheating, you naughty!!!")
		    else
		      let u1 = lk_solve { inloop = true;
                                          data = newdata1;
                                          seq = { seq with rhs = F toFocus;
                                                           formP = FSet.remove toFocus seq.formP;
                                                           formPSaved = FSet.add toFocus seq.formPSaved }}  in
		      let u2 = lk_solvef { state with toChoose = (FSet.remove toFocus state.toChoose);
                                                      nextaction = l;
                                                      dataf = newdata } in
		      ou (intercept inter_fun u1) u2
                        (fun (seqrec,pt) ->
                          match ext [seqrec] with
		          | (ga,gfP::gfPS::[])
			    -> let newseq = relevant(seq,(ga,(FSet.add toFocus gfP)::gfPS::FSet.empty::[])) in
                               (Seq.Seq newseq,Proof.one (Seq.Seq newseq) pt)
		          | _ -> (seqrec,pt))
		        (fun a->a) seq sigma cont
		        
	         | Cut(3,toCut,(newdata1,newdata), inter_fun1, inter_fun2,l) (*Focused Cut*) ->

                    if not !Flags.cuts then raise (WrongInstructionException "Cuts are not allowed");
                    if not (makes_senseF toCut seq.world)
                    then raise (WrongInstructionException "The arity of cut formula is not a prefix of current arity");
		    Dump.Kernel.incr_count 5;
                    Dump.print ["prop_search",2] (fun p->p "Cut3 on %a" (fun fmt -> IForm.print_in_fmt fmt) toCut);
                    let u1 = lk_solve { inloop = true;
                                        data   = bleft newdata1;
                                        seq    = {seq with rhs = F toCut } } in
                    let u2 = lk_solve { inloop = true;
                                        data   = bright newdata1;
                                        seq    = { seq with rhs = U(FSet.singleton (IForm.negation toCut)) }} in
                    let u3 = lk_solvef { state with nextaction = l;
                                                    dataf = newdata } in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 None seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont
                       
	         | Cut(7,toCut,(newdata1,newdata), inter_fun1, inter_fun2,l) (*Unfocused Cut*) ->
                    if not !Flags.cuts then raise (WrongInstructionException "Cuts are not allowed");
                    if not (makes_senseF toCut seq.world)
                    then raise (WrongInstructionException "The arity of cut formula is not a prefix of current arity");
		    Dump.Kernel.incr_count 5;
                    let u1 = lk_solve { inloop = true;
                                        data   = bleft newdata1;
                                        seq    = { seq with rhs = U(FSet.singleton toCut) }} in
                    let u2 = lk_solve { inloop = true;
                                        data   = bright newdata1;
                                        seq    = { seq with rhs = U(FSet.singleton (IForm.negation toCut)) }} in
                    let u3 = lk_solvef { state with nextaction = l;
                                                    dataf = newdata } in
                    ou (et (intercept inter_fun1 u1) (intercept inter_fun2 u2) (std2 None seq) seq) u3 (fun a->a) (fun a->a) seq sigma cont

	         | ConsistencyCheck(newdata,inter_fun,l) when not state.conschecked (*Checking consistency*) ->
                    let rec pythie f sigma cont =
                      Dump.Kernel.toTheory();
                      let oracle = f sigma in
                      Dump.Kernel.fromTheory();
                      cont(throw(
                               match oracle with
	                       | NoMore             -> fail seq (pythie f)
	                       | Guard(a,sigma',f') -> Success(std0(relevant(seq,(asASet a,FSet.empty::FSet.empty::FSet.empty::[]))),sigma',
                                                               fun b -> if b then pythie f' else pythie f)
                        ))
                    in
                    let u2 = lk_solvef { state with nextaction = l;
                                                    conschecked = true;
                                                    dataf = newdata } in
                    let next help =
                      ou (pythie help) u2 (fun a->a) (fun a->a) seq sigma cont
                    in
                    InsertCoin(Check(asAssign seq.lits, next))
                 (* ou (pythie (fun _ -> myconsistency atomN)) u2 (fun a->a) (fun a->a) seq sigma cont *)

	         | Polarise(l,newdata, inter_fun) when is_Und(Pol.iatom seq.polar l) ->
                    let u = lk_solve { inloop = false;
                                       data   = newdata; 
                                       seq    = { seq with polar = Pol.declarePos seq.polar l }}  in
		    straight (fun a->a) seq (intercept inter_fun u) sigma cont
                             
	         | DePolarise(l,newdata, inter_fun) when not (is_Und(Pol.iatom seq.polar l)) ->
                    if not !Flags.depol
                    then raise (WrongInstructionException "Depolarisation is not allowed");
		    let u = lk_solve { inloop = false;
                                       data   = newdata;
                                       seq    = { seq with polar = Pol.remove seq.polar l }}  in
		    straight (fun a->a) seq (intercept inter_fun u) sigma cont
                             
	         | Propose(NotProvable s) when (Seq.subseq seq s) ->
                    cont (throw (fail s (lk_solvef { state with nextaction = (fun ()->None) } )))
                         
	         | Propose(Provable(s,pt,sigma')) when (Seq.subseq s seq) ->
                    let resume = lk_solvef { state with nextaction = (fun ()->None) } in
                    (match Constraint.meet sigma' sigma with
                     | None         -> straight (fun a->a) seq resume sigma cont
                     | Some sigma'' -> cont (throw (Success(Genuine(Seq.Seq s,pt),sigma'', fun _ -> resume)))
                    )
                      
	         | Get(true,b2,l) ->
                    cont (Success(Fake([%eq:bool] !dir b2),sigma,fun _ -> lk_solvef { state with nextaction = l }))

	         | Get(false,b2,l) ->
                    cont (Fail(Fake([%eq:bool] !dir b2),fun _ -> lk_solvef { state with nextaction = l }))
                           
	         | Restore(newdata,inter_fun,l) when not (FSet.is_empty seq.formPSaved)  ->
                    if !Flags.fair && not (FSet.is_empty state.toChoose)
		    then raise (WrongInstructionException "Trying to restore formulae on which focus has already been placed,
                                                           but there still are formulae that you have not tried;
                                                           your treatment is unfair");
                    Dump.print ["prop_search",1] (fun p->p "Restoring");
                    let newseq = { seq with formP = FSet.union seq.formP seq.formPSaved;
                                            formPSaved = FSet.empty } in
		    let u = lk_solvef { state with toChoose = FSet.union state.toChoose seq.formPSaved;
                                                   nextaction = l;
                                                   dataf = newdata;
                                                   seqf = newseq }
                    in straight (fun a->a) seq (intercept inter_fun u) sigma cont

	         | _ -> raise (WrongInstructionException "focus_pick has suggested a stupid action")

	     in match state.nextaction() with
	        | Some(action)-> Dump.Kernel.toPlugin();
                                 action_analysis action
	        | None        -> (Dump.Kernel.toPlugin();
                                  InsertCoin(AskFocus(seq,
                                                      sigma,
                                                      state.toChoose,
                                                      not (FSet.is_empty seq.formPSaved),
                                                      state.conschecked,
                                                      action_analysis,
                                                      state.dataf)))
	 in
	 let newcont inter_fun loc_ans =
           (match prune loc_ans with Some a -> inter_fun a
                                   | None -> ());
           cont loc_ans 
	 in
	 let notify_analysis(accept_defeat,newdata,inter_fun,action1) =
           Dump.Kernel.fromPlugin();
	   if state.inloop && accept_defeat
           then (Dump.Kernel.incr_count 7;
                 cont (throw(fail seq (lk_solve state))))
	   else let statef = {
                    seqf = seq;
                    toChoose = seq.formP;
                    conschecked = false;
                    nextaction = action1;
                    dataf = newdata
                  }
                in
                lk_solvef statef sigma (newcont inter_fun)
	 in
	 Dump.Kernel.incr_count 8;
         Dump.Kernel.toPlugin();
	 InsertCoin(Notify(seq,sigma,state.inloop,notify_analysis,state.data))

	           
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
        | Success(Genuine(Seq.Seq seq,pt),sigma,_) ->
           begin
             match seq.rhs with
             | U _ -> fin "PROVABLE"; Jackpot(Provable(seq,pt,sigma))
             | F _ -> failwith "Sequent should be unfocussed!"
           end
        | Fail(Genuine(Seq.Seq seq),_) -> 
           begin
             match seq.rhs with
             | U _ -> fin "NOT PROVABLE"; Jackpot(NotProvable seq)
             | F _ -> failwith "Sequent should be unfocussed!"
           end
        | Success(Fake b2,sigma,f) -> 
           dir:= not !dir ;
           let strg = if b2 then "right" else "left" in
           Dump.print ["prop_search",1] (fun p->p "No more Success branch on the %s" strg);
           InsertCoin(Stop(true,b2,fun _ -> wrap (f true)))
        | Fail(Fake b2,f)          -> 
           dir:= not !dir ;
           let strg = if b2 then "right" else "left" in
           Dump.print ["prop_search",1] (fun p->p "No more Failure branch on the %s" strg);
           InsertCoin(Stop(false,b2,fun _ -> wrap (f true)))
      in
      f Constraint.topconstraint inter

    (* Wraps the above function by providing initial inloop and
       initial sequent *)

    let machine_seq seq data = Dump.Kernel.init();
                               wrap (lk_solve { inloop = false;
                                                data   = data;
                                                seq    = seq } )

    let machine formula init_data = 
      let seq =
        { lits = ASet.empty;
          rhs = U(FSet.singleton formula);
          formP = FSet.empty;
          formPSaved = FSet.empty;
          polar = Pol.empty;
          world = World.init }
      in machine_seq seq (init_data seq)

  end

end
