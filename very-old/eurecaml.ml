(* Active le debug (affichage des fails) *)
let debug = 0;;
let failcount = ref 0;;
let tiredcount = ref 0;;
let successcount = ref 0;;

(* 
 * Un zipper est une structure de stockage de donnée representé par deux piles :
 * la droite et la gauche. Le "curseur" représente la jonction entre ces deux 
 * piles
 *)
type 'a zipper = Zip of ('a list)*('a list);;

(* Construit un zipper vide *)
let empty_zip = Zip([],[]);;

(* Verifie si le curseur est au tout debut du zipper *)
let is_home = function
    Zip([],_)  -> true
  | _       -> false
;;

(* 
 * Verifie si le curseur est a la fin. On utilise pas is_end car c'est une
 * fonction reservee 
 *)
let is_out = function
    Zip(_,[]) -> true
  | _         -> false
;;

(* Renvoie l'element suivant (de droite) et le zipper - cet element *)
let next = function
    Zip(l,f::r)   -> Zip(l,r),f
  | _             -> failwith "Plus d'elements"
;;


(* Renvoie l'element precedent (de gauche) et le zipper - cet element *)
let previous = function
    Zip(f::l,r)   -> Zip(l,r),f
  | _             -> failwith "Plus d'elements"
;;

let get_left = function
    Zip(l,r) -> l
;;

(* Ajoute un element sur la pile de gauche *)
let add_left x = function
    Zip(l,r)  -> Zip(x::l,r)
;;

(* Ajoute un element sur la pile de droite *)
let add_right x = function
    Zip(l,r)  -> Zip(l,x::r)
;;

(* Ajoute un element à la fin de la pile de droite *)
let rec add_end x = function
    Zip(l,[]) -> Zip(l, x::[])
  | Zip(l,y::r) -> add_end x (Zip(y::l,r))
;;

let rec fusion_zip z1 = function
    Zip([], []) -> z1
  | Zip(a::l, r) -> fusion_zip (add_left a z1) (Zip(l, r))
  | Zip(l, a::r) -> fusion_zip (add_right a z1) (Zip(l, r))
;;

(* Deplace le curseur au debut de la pile de gauche *)
let rec home = function
    Zip([],r)   -> Zip([],r)
  | Zip(f::l,r) -> home(Zip(l,f::r))
;;

(* Deplace le curseur a la fin de la pile de droite *)
let rec zipend = function
    Zip(l, [])   -> Zip(l, [])
  | Zip(l, f::r) -> zipend(Zip(f::l,r))
;;

(* Verifie si un zipper est vide *)
let is_empty = function
    Zip([], []) -> true
  | _ -> false 
;;

(* Verifie l'appartenance d'un element au zipper *)
let rec is_in_zipper x = function
    Zip([], []) -> false  
  | Zip(y::l, r) when !y=x -> true
  | Zip(y::l, r) -> is_in_zipper x (Zip(l, r))
  | Zip(l, y::r) when !y=x -> true
  | Zip(l, y::r) -> is_in_zipper x (Zip(l, r))
;;

let is_empty_list = function 
    [] -> true
  | _ -> false
;;

let rec is_in_list x = function
    [] -> false  
  | y::l when !y=x -> true
  | y::l -> is_in_list x l
;;



(* 
 * Un term est une variable ('p') ou une fonction avec des arguments ('f(a,b)')
 * On peut imbriquer les fonctions, bien entendu.
 *)
type term = V of string | C of string*(term list);;


(*
 * Une formule peut etre positive ou negative.
 * Elle est composee d'atomes ou de Et(form1, form2) ou de Ou(form1, form2)
 *)
type formula = Pos of posFormula | Neg of negFormula | Und of undFormula
and
  posFormula = 
    PosAtom of string*(term list) 
  | AndP of formula*formula
  | OrP of formula*formula
and
  negFormula = 
    NegAtom of string*(term list)
  | AndN of formula*formula
  | OrN of formula*formula
and
  undFormula = 
    PosAtomU of string*(term list)
  | NegAtomU of string*(term list)
  | AndU of formula*formula
  | OrU of formula*formula
;;


(* 
 * perp inverse la polarité d'une expression
 *)
let rec
    perp: formula->formula = function
	Pos p -> Neg (perpP p)
      | Neg n -> Pos (perpN n)
      | Und n -> Und (perpU n)
and
    perpP:posFormula->negFormula = function
	PosAtom(f,tl) -> NegAtom(f,tl) 
      | AndP(a,b)     -> OrN(perp a,perp b)
      | OrP(a,b)      -> AndN(perp a,perp b)
and
    perpN:negFormula->posFormula = function
	NegAtom(f,tl) -> PosAtom(f,tl) 
      | AndN(a,b)     -> OrP(perp a,perp b)
      | OrN(a,b)      -> AndP(perp a,perp b)
and
    perpU:undFormula->undFormula = function
	PosAtomU(f,tl) -> NegAtomU(f,tl) 
      |	NegAtomU(f,tl) -> PosAtomU(f,tl) 
      | AndU(a,b)     -> OrU(perp a,perp b)
      | OrU(a,b)      -> AndU(perp a,perp b)
;;

(*
 * Check les deux listes pour trouver une concordance
 *)
let rec 
    equalsT l = function
	(V x,V y) when x=y -> true
      | (V x,V y) -> 
	  begin
	    match l with
		[] -> false
	      | (x',y')::l' when (x'=x && y'=y)-> true
	      | (x',y')::l' -> equalsT l' (V x,V y)
	  end
      | (C(f,tl),C(g,ul)) when f=g -> equalsTl l (tl,ul)
      | _ -> false
and
    equalsTl l = function
	([],[]) -> true
      | (t::tl',u::ul') -> (equalsT l (t,u))&&(equalsTl l (tl',ul'))
      | _ -> false
;;


(*
 * Verifie la concordance entre deux formules.
 *)
let rec
    equalsP l = function
	(PosAtom(f,tl),PosAtom(g,ul)) -> (f=g) && (equalsTl l (tl,ul))
      | (AndP(a,b),AndP(a',b')) -> (equalsF l (a,a')) && (equalsF l (b,b'))
      | (OrP(a,b),OrP(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsN l = function
	(NegAtom(f,tl),NegAtom(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      | (AndN(a,b),AndN(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | (OrN(a,b),OrN(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsU l = function
	(PosAtomU(f,tl),PosAtomU(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      |	(NegAtomU(f,tl),NegAtomU(g,ul)) -> (f=g)&&(equalsTl l (tl,ul))
      | (AndU(a,b),AndU(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | (OrU(a,b),OrU(a',b')) -> (equalsF l (a,a'))&&(equalsF l (b,b'))
      | _ -> false
and
    equalsF l = function
	(Pos p,Pos q) -> equalsP l (p,q)
      | (Neg n,Neg m) -> equalsN l (n,m)
      | (Und n,Und m) -> equalsU l (n,m)
      | _ -> false
;;

(*
Forces polarity 
*)

let rec
    forcepolP at pos = function
	PosAtom(f,tl) when f<>at -> PosAtom(f,tl)
      | AndP(a,b) -> AndP(forcepolF at pos a,forcepolF at pos b)
      | OrP(a,b) -> OrP(forcepolF at pos a,forcepolF at pos b)
      | _ -> failwith("Polarity of "^at^" is already fixed")
and
    forcepolN at pos = function
	NegAtom(f,tl) when f<>at -> NegAtom(f,tl)
      | AndN(a,b) -> AndN(forcepolF at pos a,forcepolF at pos b)
      | OrN(a,b) -> OrN(forcepolF at pos a,forcepolF at pos b)
      | _ -> failwith("Polarity of "^at^" is already fixed")
and
    forcepolU at pos = function
	NegAtomU(f,tl) when f<>at -> Und(NegAtomU(f,tl))
      | NegAtomU(f,tl) -> if pos then Neg(NegAtom(f,tl)) else Pos(PosAtom(f,tl))
      |	PosAtomU(f,tl) when f<>at -> Und(NegAtomU(f,tl))
      | PosAtomU(f,tl) -> if pos then Pos(PosAtom(f,tl)) else Neg(NegAtom(f,tl))
      | AndU(a,b) -> Und(AndU(forcepolF at pos a,forcepolF at pos b))
      | OrU(a,b) -> Und(OrU(forcepolF at pos a,forcepolF at pos b))
and
    forcepolF at pos = function
	Pos p -> Pos(forcepolP at pos p)
      |	Neg n -> Neg(forcepolN at pos n)
      |	Und n -> forcepolU at pos n
;;

let make_pos = function
    	NegAtomU(f,tl) -> PosAtom(f,tl)
      |	PosAtomU(f,tl) -> PosAtom(f,tl)
      | AndU(a,b) -> AndP(a,b)
      | OrU(a,b) -> OrP(a,b)
;;

let make_neg = function
    	NegAtomU(f,tl) -> NegAtom(f,tl)
      |	PosAtomU(f,tl) -> NegAtom(f,tl)
      | AndU(a,b) -> AndN(a,b)
      | OrU(a,b) -> OrN(a,b)
;;

(* transforms list and zipper *)

let rec
    applylist  transform = function
	[] -> []
      | f::l ->   (transform f)::(applylist  transform l)
;;

let applyzipper  transform = function
    Zip(l,r) -> Zip(applylist transform l,applylist transform r)
;;


(* List d'atomes negatifs * FormuleFocusee * Liste de formules positives * Formules positives saved*)
type seq = 
    EntF of (((string*(term list)) ref) zipper)*formula*((posFormula ref) zipper)*((posFormula ref) list)
  | EntUF of (((string*(term list)) ref) zipper)*(formula zipper)*((posFormula ref) zipper)*((posFormula ref) list);;

(*
 * Structure d'arbre de preuve
 *)
type prooftree = 
    Axiom of seq 
  | OnePre of seq*prooftree 
  | TwoPre of seq*prooftree*prooftree
;;
(*
 * Renvoie un proovtree si le théorême est vrai
 *)
type answer = Success of prooftree | Fail of (((unit -> answer)* seq) zipper);;



(* Affiche une term list *)
let rec printtl = function
    [] -> ""
  | t::[] -> printt(t);
  | t::l -> printt(t)^", "^printtl(l);
and 
    printt = function
	V(a) -> a;
      | C(f, newtl) -> f^"( "^printtl(newtl)^")";
;;


let rec printformula = function
    Neg(a) -> printformulaN(a)
  | Pos(a) -> printformulaP(a)
  | Und(a) -> printformulaU(a)
and
    printformulaN = function
	NegAtom(s, tl) -> "\\non {"^s^"("^printtl(tl)^")}";
      | AndN(f1,f2) -> "("^printformula(f1)^") \\andN ("^printformula(f2)^")";
      | OrN(f1,f2) -> "("^printformula(f1)^") \\orN ("^printformula(f2)^")";
and
    printformulaP = function
	PosAtom(s, tl) -> s^"("^printtl(tl)^")";
      | AndP(f1,f2) -> "("^printformula(f1)^") \\andP ("^printformula(f2)^")";
      | OrP(f1,f2) -> "("^printformula(f1)^") \\orP ("^printformula(f2)^")";
and
    printformulaU = function
	PosAtomU(s, tl) -> s^"?("^printtl(tl)^")";
      |	NegAtomU(s, tl) -> "\\non {"^s^"?("^printtl(tl)^")}";
      | AndU(f1,f2) -> "("^printformula(f1)^") \\wedge ("^printformula(f2)^")";
      | OrU(f1,f2) -> "("^printformula(f1)^") \\vee ("^printformula(f2)^")";
;;

let rec printl b pprint = function
    [] -> ""
  | f::[] -> pprint(f);
  | f::l when b -> pprint(f)^", "^(printl b pprint l);
  | f::l -> (printl b pprint l)^", "^pprint(f);
;;

let printzipper pprint separator= function
    Zip(l, r) -> (printl false pprint l)^separator^(printl true pprint r);

;;

let rec printseq = function
    EntUF(atomsN, unfocused, formuP, formuPSaved)
    -> " \\DerOSNeg {"^
      (printzipper (function x-> match !x with (s, tl) -> s^"("^printtl(tl)^")") " \\cdot " atomsN)^
      "} {"^(printzipper printformula " \\cdot " unfocused)^"}"^
      "{"^(printzipper (function x -> printformulaP (!x)) " \\cdot " formuP)^"\\mid "^
      (printl true (function x -> printformulaP (!x)) formuPSaved)^"}";
      
  | EntF(atomsN, focused, formuP, formuPSaved)
    -> " \\DerOSPos {"^(printzipper (function x-> match !x with  (s, tl) -> s^"("^printtl(tl)^")") " \\cdot " atomsN)^
      "} {"^(printformula focused)^"}"^
        "{"^(printzipper (function x -> printformulaP (!x)) " \\cdot " formuP)^"\\mid "^
        (printl true (function x -> printformulaP (!x)) formuPSaved)^"}"
;;

(* Affiche un prooftree en latex *)
let rec printprooftree = function
    OnePre (a,b) -> "\\infer {"^(printseq a)^"}{"^printprooftree(b)^"}";
  | TwoPre (a,b,c) -> "\\infer {"^(printseq a)^
      "}{"^printprooftree(b)^
        " \\quad "^printprooftree(c)^"}";
  | Axiom (a) -> "\\infer {"^(printseq a)^"}{}"         
;;


let printanswer = function
    Success( p ) -> "$$"^printprooftree p^"$$";
  | Fail(x) -> "\\textsf {FAIL} \\\\"^printzipper (function (y, z) -> "$$"^(printseq z)^"$$") " " (x)
;;


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



let make_new_cont cont x = function
    Success(prooftree) -> cont (Success(prooftree))
  | Fail(y) -> cont (Fail (fusion_zip x y))
;;


(*
 * Fonction principale 
 * delta = les formules restantes a analyser 
 * gammatomN = Atoms negatifs trouves
 * gammaformP = Formules positives trouves
 * gammaformPSaved = Formules sauvegardees (continuation)
 * Renvoie un Success(Prooftree) si la preuve est faite
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



open Printf;;

let write_to_file = fun filename s ->
  let chan = open_out filename in 
    fprintf chan "%s\n" s;  
    close_out chan; 
;;




(* Tests *)

(* p(x) \/- !p(x) *)
let my_f1 = Neg(
  OrN(
    Pos(
      PosAtom("p",
              [V("x")])
    ),
    Neg(
      NegAtom("p",
              [V("x")])
    )
  )
)
;;

(* p(x) \/+ !p(x) *)
let my_f2 = Pos(
  OrP(
    Pos(
      PosAtom("p",
              [V("x")])
    ),
    Neg(
      NegAtom("p",
              [V("x")])
    )
  )
)
;;

(* !p(x) \/+ p(x) : tourne en boucle si recherche de preuve en profondeur*)
let my_f3 = Pos(
  OrP(
    Neg(
      NegAtom("p",
	      [])
    ),
    Pos(
      PosAtom("p",
	      [])
    )
  )
)
;;

(* (a \/- b) \/- (!a /\- !b) *)

let my_f4 = Neg(
  OrN(
    Neg(
      OrN(
        (Pos(
           PosAtom("a", []))),
        (Pos(
           PosAtom("b", [])))
      )
    ),
    Neg(
      AndN(
        Neg(
          NegAtom("a", [])), 
        Neg(
          NegAtom("b", []))
      )
    )
  )
)
;; 

(* (a \/+ b) \/- (!a /\- !b) *)
let my_f5 = Pos(
  OrP(
    Pos(
      OrP(
        (Pos(
           PosAtom("a", []))),
        (Pos(
           PosAtom("b", [])))
      )
    ),
    Neg(
      AndN(
        Neg(
          NegAtom("a", [])), 
        Neg(
          NegAtom("b", []))
      )
    )
  )
)
;; 

(* (!a \/+ !b) pas prouvable - algo naif tourne en boucle *)
let my_f6 = Pos(
  OrP(
    Neg(
      NegAtom("a", [])), 
    Neg(
      NegAtom("b", []))
  )
)
;; 

(* old code
let my_z = add_left my_f5 empty_zip;;
let my_e = EntUF(empty_zip, my_z, empty_zip, []);;
let my_e = EntUF((add_left ("p",[]) empty_zip), my_z, empty_zip, []);;
*)

let orig_seq my_formula = EntUF(empty_zip,(add_left my_formula empty_zip), empty_zip, []);;

let orig_situation my_seq = fun () -> lk_solve (fun x->x) false my_seq;;

let go formula = 
  print_endline("===");
  failcount := 0;
  tiredcount := 0;
  successcount := 0;
  let h = orig_seq formula in solve (add_left (orig_situation h,h) empty_zip);;

write_to_file "latex/eurecaml.tex" (printanswer (go my_f1)^"\\vspace{30pt}"^
				      (printanswer (go my_f2))^"\\vspace{30pt}"^
				      (printanswer (go my_f3))^"\\vspace{30pt}"^
				      (printanswer (go my_f4))^"\\vspace{30pt}"^
				      (printanswer (go my_f5))^"\\vspace{30pt}"^
				      (printanswer (go my_f6))
				   )
;;


let read_from_file filename =
  let lines = ref "" in
  let chan = open_in filename in
    try
      while true; do
	lines := (!lines)^"\n"^(input_line chan)
      done; ""
    with End_of_file ->
      close_in chan;
      !lines;
;;

open String;;

let rec list_from_string s list_so_far n = 
  if (n>=length s) then List.rev list_so_far 
  else
    match s.[n] with 
	' '  -> list_from_string s list_so_far (n+1)
      |	'\n' -> list_from_string s list_so_far (n+1)
      | '-'  -> list_from_string s ("-"::list_so_far) (n+1)
      |  _   -> let rec word_from_string s word_so_far n =
	   if (n>=length s) then List.rev (word_so_far::list_so_far) 
	   else
	     begin
	       match s.[n] with
		   ' '  -> begin
		     list_from_string s (word_so_far::list_so_far) n
		       end
		 | '\n' -> list_from_string s (word_so_far::list_so_far) n
		 | c    -> word_from_string s (word_so_far^(Char.escaped c)) (n+1)
	     end
	 in
	   word_from_string s "" n
;;

let rec print_list = function
    [] -> ();
  | s::l -> print_string (s^" "); print_list l;;

	     
let rec parse_cnf cnf_so_far = function
    []     -> List.rev cnf_so_far
  | "0"::l -> parse_cnf cnf_so_far l
  | l -> let rec parse_clause clause_so_far ispos = function
	[]     -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) []
      | "0"::l -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) l
      | "-"::l -> parse_clause clause_so_far false l
      | s::l when ispos -> parse_clause ((true,s)::clause_so_far) true l
      | s::l   -> parse_clause ((false,s)::clause_so_far) true l
    in parse_clause [] true l
;;

let rec parse_cnf_file = function
    []     -> []
  | "cnf"::_::_::l -> parse_cnf [] l
  | a::l -> parse_cnf_file l
;;

let generate_atom = function
    (true,t)  -> Und(PosAtomU (t, []))
  | (false,t) -> Und(NegAtomU (t, []))
;;

let rec generate_clause =  function
    t::[] -> generate_atom t
(*    | t::l  -> if (Random.bool())*)
  | t::l  -> if (false)  
    then Pos(OrP((generate_atom t),(generate_clause l)))
    else Neg(OrN((generate_atom t),(generate_clause l)))
  | []    -> Pos(AndP(Pos(PosAtom ("p",[])),Neg(NegAtom ("p",[]))))
;;


let rec generate_cnf =  function
    t::[] -> generate_clause t
  | t::l  -> if (true)  
    then Pos(AndP((generate_clause t),(generate_cnf l)))
    else Neg(AndN((generate_clause t),(generate_cnf l)))
  | []    -> Pos(OrP(Pos(PosAtom ("p",[])),Neg(NegAtom ("p",[]))))
;;

(*


let treatfile filename = print_list  (list_from_string (read_from_file(filename)) [] 0);;

*)

let treatfile filename =  print_string ((read_from_file(filename)));;
let treatfile filename = print_endline(printformula (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0)))));;
let treatfile filename = go (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0))));;


let treatdir a = 
  let b = Sys.readdir(a) in
  for i=0 to Array.length b-1 do
    treatfile (a^Filename.dir_sep^b.(i));
    done
;;

(* treatdir("sat-2002-beta/generated/gen-9/gen-9.1");; *)

treatfile "test.cnf";;
(* write_to_file "latex/eurecaml.tex" (printanswer (treatfile "test.cnf"));;*)
