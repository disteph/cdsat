open Sequents;;
include Sequents;;

(* Activates debug mode (displays fails, etc) *)
let debug = 2;;
let failcount = ref 0;;
let tiredcount = ref 0;;
let successcount = ref 0;;


let throwfail s = 
  failcount := (!failcount)+1;
  if debug>0  then 
      if (((!failcount) mod 500) ==0) then 
	begin
	  print_int(!failcount);
	  if debug==1 then print_endline(" Fail : "); 
	  if debug>1  then print_endline(" Fail : "^printseq s);
	end;
  Fail(empty_zip);
;;


let throwtired s a= 
  tiredcount := (!tiredcount)+1;
  if debug>0  then 
      if (((!tiredcount) mod 500) ==0) then 
	begin
	  print_int(!tiredcount);
	  if debug==1 then print_endline(" Tired : ");
	  if debug>1  then print_endline(" Tired : "^printseq s);
	end; 
  Fail(add_left (a, s) empty_zip)
;;

let throwsuccess s= 
  successcount := (!successcount)+1;
  if debug>0  then 
      if (((!successcount) mod 500) ==0) then 
	begin
	  print_int(!successcount);
	  if debug==1 then print_endline(" Success : ");
	  if debug>1  then print_endline(" Success : "^printseq s);
	end;
  Success(Axiom(s))
;;


(*
Updating a continuation in order to merge previous tired points (x) with newly obtained tired points (y).
*)

let make_new_cont cont x = function
    Success(prooftree) -> cont (Success(prooftree))
  | Fail(y) -> cont (Fail (fusion_zip x y))
;;


(*
 * Main Search function 
 * delta = Formulae to be asynchronously decomposed 
 * gammatomN = negative atoms found in asynchronous phase (negation symbol not stored)
 * gammaformP = positive formulae found in asynchronous phase (focus to be placed on them later)
 * gammaformPSaved = positive formulae on which focus has been placed more times than remaining formulae in gammaformP 
 * (These other formulae have priority for focus -> ensures fairness)
 * Returns Success(Prooftree) if a proof is found
 *)

let rec lk_solve cont inloop = function
    EntUF(gammatomN, delta, gammaformP, gammaformPSaved) when is_empty (delta) 
      -> begin 
        if ((inloop)||((is_out (gammaformP)) && (is_empty_list (gammaformPSaved)))) 
	then cont (throwfail (EntUF(gammatomN, delta, gammaformP, gammaformPSaved))) 
	else
          if (is_out (gammaformP)) then 
            let newcont = function 
                Success(prooftree) -> cont (Success (prooftree))
              | Fail(x) -> Fail(x) in
            let newseq = EntUF(gammatomN, delta, home (Zip(gammaformPSaved, get_left gammaformP)), []) in
              cont (throwtired newseq (function () -> lk_solve newcont false (newseq))) 
	  else
            let (newgammaformP, toFocus) = next(gammaformP) in 
            let newcont = function
                Success(prooftree) -> cont (Success(OnePre (EntUF(gammatomN, delta, gammaformP, gammaformPSaved), prooftree)))
              | Fail(x) -> lk_solve (make_new_cont cont x) false (EntUF (gammatomN, delta, add_left toFocus newgammaformP, gammaformPSaved))  in
              lk_solve newcont true (EntF (gammatomN, Pos (!toFocus), home (newgammaformP), toFocus::gammaformPSaved))
      end

  | EntUF(gammatomN, delta, gammaformP, gammaformPSaved) when is_out (delta) 
      -> lk_solve cont inloop (EntUF(gammatomN, home(delta), gammaformP, gammaformPSaved))

  | EntUF(gammatomN, delta, gammaformP, gammaformPSaved)
    -> let (newdelta, toDecompose) = next (delta) in
      begin
        match toDecompose with
            Pos p -> let newcont = function
                Success(prooftree) -> cont (Success(OnePre (EntUF(gammatomN, delta, gammaformP, gammaformPSaved), prooftree)))
              | Fail(x) -> cont (Fail(x)) in
	      if ((is_in_zipper p gammaformP)||(is_in_list p gammaformPSaved))
	      then lk_solve newcont inloop (EntUF (gammatomN, newdelta, gammaformP,gammaformPSaved))
	      else lk_solve newcont false (EntUF (gammatomN, newdelta, add_right (ref p) gammaformP,gammaformPSaved))

          | Neg (NegAtom (f, tl)) -> let newcont = function 
                Success(prooftree) -> cont (Success(OnePre (EntUF(gammatomN, delta, gammaformP, gammaformPSaved), prooftree)))
              | Fail(x) -> cont (Fail(x)) in
              if (is_in_zipper (f,tl) gammatomN)
	      then lk_solve newcont inloop (EntUF (gammatomN,newdelta, gammaformP, gammaformPSaved))
	      else lk_solve newcont false (EntUF (add_right (ref (f, tl)) gammatomN,newdelta, gammaformP, gammaformPSaved))

          | Neg (AndN (a1, a2)) -> 
              let newcont1 = function
                  Success(prooftree1) -> let newcont2 = function
                      Success(prooftree2) -> cont (Success(TwoPre (EntUF(gammatomN, delta, gammaformP, gammaformPSaved), prooftree1, prooftree2)))
                    | Fail(x) -> cont (Fail(x)) in
                    lk_solve newcont2 inloop (EntUF (gammatomN, add_left a2 newdelta, gammaformP, gammaformPSaved))
                      
                | Fail(x) -> cont (Fail(x)) in                 
                lk_solve newcont1 inloop (EntUF (gammatomN, add_left a1 newdelta, gammaformP, gammaformPSaved))
                  
          | Neg (OrN (a1, a2)) -> let newcont = function
                Success(prooftree) -> cont (Success(OnePre (EntUF(gammatomN, delta, gammaformP, gammaformPSaved), prooftree)))
              | Fail(x) -> cont (Fail(x)) in
              lk_solve newcont inloop (EntUF (gammatomN, add_left a2 (add_left a1 newdelta), gammaformP, gammaformPSaved))
	  | Und(PosAtomU(f, tl)) 
	    -> lk_solve cont false (EntUF(
				      add_right (ref (f, tl)) gammatomN, 
				      applyzipper (forcepolF f false) newdelta, 
				      applyzipper (function x-> ref (forcepolP f false !x)) gammaformP, 
				      applylist (function x-> ref (forcepolP f false !x)) gammaformPSaved))
	  | Und(NegAtomU(f, tl)) 
	    -> lk_solve cont false (EntUF(
				      add_right (ref (f, tl)) gammatomN, 
				      applyzipper (forcepolF f true) newdelta, 
				      applyzipper (function x-> ref (forcepolP f true !x)) gammaformP, 
				      applylist (function x-> ref (forcepolP f true !x)) gammaformPSaved))
	  | Und(a) 
	    -> lk_solve cont inloop (EntUF(gammatomN, add_left (Neg(make_neg (a))) newdelta, gammaformP, gammaformPSaved))

      end
        
  | EntF(gammatomN, Neg(a), gammaformP, gammaformPSaved) 
    -> let newcont = function
        Success(prooftree) -> cont (Success(OnePre (EntF(gammatomN, Neg(a), gammaformP, gammaformPSaved), prooftree)))
      | Fail(x) -> cont (Fail(x)) in
      lk_solve newcont inloop (EntUF (gammatomN, add_right (Neg(a)) empty_zip, gammaformP, gammaformPSaved))
        
  | EntF(gammatomN, Pos(AndP(a1, a2)), gammaformP, gammaformPSaved)
    -> let newcont1 = function
        Success(prooftree1) -> let newcont2 = function
            Success(prooftree2) -> cont (Success(TwoPre (EntF(gammatomN, Pos(AndP(a1, a2)), gammaformP, gammaformPSaved), prooftree1, prooftree2)))
          | Fail(x) -> cont (Fail(x)) in
          lk_solve newcont2 inloop (EntF (gammatomN, a2, gammaformP, gammaformPSaved))
      | Fail(x) -> cont (Fail(x)) in
      lk_solve newcont1 inloop (EntF (gammatomN, a1, gammaformP, gammaformPSaved))
        
  | EntF(gammatomN, Pos(OrP(a1, a2)), gammaformP, gammaformPSaved)
    -> let newcont2 = function
        Success(prooftree2) -> cont (Success(OnePre (EntF(gammatomN, Pos(OrP(a1, a2)), gammaformP, gammaformPSaved), prooftree2)))
      | Fail(x) -> cont (Fail(x)) in
    let newcont1 = function
        Success(prooftree1) -> cont (Success(OnePre (EntF(gammatomN, Pos(OrP(a1, a2)), gammaformP, gammaformPSaved), prooftree1)))
      | Fail(x) -> lk_solve (make_new_cont newcont2 x) inloop (EntF (gammatomN, a2, gammaformP, gammaformPSaved)) in
      lk_solve newcont1 inloop (EntF (gammatomN, a1, gammaformP, gammaformPSaved))

  | EntF(gammatomN, Pos(PosAtom(f, tl)), gammaformP, gammaformPSaved) 
    -> if (is_in_zipper (f, tl) gammatomN) 
    then cont (throwsuccess (EntF(gammatomN, Pos(PosAtom(f, tl)), gammaformP, gammaformPSaved))) 
    else cont (throwfail (EntF(gammatomN, Pos(PosAtom(f, tl)), gammaformP, gammaformPSaved)))
  | EntF(gammatomN, Und(PosAtomU(f, tl)), gammaformP, gammaformPSaved) 
    -> lk_solve cont false (EntF(
			      gammatomN, 
			      Neg(NegAtom(f, tl)), 
			      applyzipper (function x-> ref (forcepolP f false !x)) gammaformP, 
			      applylist (function x-> ref (forcepolP f false !x)) gammaformPSaved))
  | EntF(gammatomN, Und(NegAtomU(f, tl)), gammaformP, gammaformPSaved) 
    -> lk_solve cont false (EntF(
			      gammatomN, 
			      Neg(NegAtom(f, tl)), 
			      applyzipper (function x-> ref (forcepolP f true !x)) gammaformP, 
			      applylist (function x-> ref (forcepolP f true !x)) gammaformPSaved))
  | EntF(gammatomN, Und(a), gammaformP, gammaformPSaved) 
    -> lk_solve cont inloop (EntF(gammatomN, Pos(make_pos (a)), gammaformP, gammaformPSaved))
;;

let rec solve toresurect = 
  (*     print_endline(printzipper (function (y, z) -> (printseq z)) " " (toresurect));*)
  if is_empty (toresurect) then 
    begin
      print_endline("Total failure, with "^(string_of_int !failcount)^" Fails and "^(string_of_int !tiredcount)^" Tireds");
      Fail(empty_zip)
    end
  else if is_out(toresurect) then
    solve(home toresurect)
  else
    let (c, (a, b)) = next(toresurect) in
      if debug>0 then print_string("Je (re)prend maintenant la piste : ");
      if debug==1 then print_endline("");
      if debug>1 then print_endline(printseq b);
      match a() with
          Success(prooftree) -> print_endline("Success, with "^(string_of_int !failcount)^" Fails and "^(string_of_int !tiredcount)^" Tireds");Success(prooftree)
        | Fail(x) -> let newtoresurect = (fusion_zip c (zipend x)) in
            solve(newtoresurect)
;;
