(* Implementation of CC(X) *)

open Top
open Specs

open Interfaces

module Algo 
  (DS: GTheoryDSType)
  (X:SolvableTheory with type VtoTSet.v = DS.TSet.t
                    and  type t = DS.Term.t)
  (U:PersistentUnionFind with type e = X.v) = 
struct

  let directSubterms t = 
    match Terms.reveal t with
    | Terms.V x    -> []
    | Terms.C(f,l) -> l

  let root t = 
    match Terms.reveal t with
    | Terms.V x    -> None
    | Terms.C(f,l) -> Some f
  
  open X
  open DS

  (* let inputToString = function *)
  (*   | (Eq(a,b)) -> (toString a)^" = "^(toString b) *)
  (*   | (NEq(a,b)) -> (toString a)^" <> "^(toString b) *)
  (*   | (Congr(a,b)) -> (toString a)^" c "^(toString b) *)
      
  (* let print_input i = print_string (inputToString i) *)

  (* let print_inputlist l = *)
  (*   let rec aux s = function *)
  (*     | [] -> print_string s *)
  (*     | (a::ta) -> aux (s^(inputToString a)^" | ") ta *)
  (*   in aux "" l *)

  type state = {theta : TSet.t;
                gamma : VtoTSet.t;
                delta : VtoV.t; 
                n     : (Sorts.t*Term.t*Term.t) list;
                sub   : ((X.v * X.v) * Term.t input) list;
                u     : Term.t input U.t
               }

  exception Inconsistency of Term.t input list

(* verify if our equivalence classes or coherent with a set of disequalities
   if not : output the 'bad' disequality *)
  let rec coherent delta = function 
    | [] -> None
    | ((_,a,b) as h)::t when X.vequal (VtoV.find (make a) delta) (VtoV.find (make b) delta)
                        -> Some h
    | _::t -> coherent delta t

(* verify if the terms are in the same equivalence class 2 by 2 *)
  let rec equal l l' delta =
    match l,l' with
    | [], [] -> true
    | a::ta, b::tb when X.vequal (VtoV.find (make a) delta) (VtoV.find (make b) delta)
        -> equal ta tb delta
    | _ -> false

  exception Finished

(* find a subterm such that it is not in theta but all its direct subterms are *)
  let rec findSterm t theta =
    try aux theta (directSubterms t) with Finished -> t
  and aux theta = function
    | [] -> raise Finished
    | a::ta when TSet.mem a theta -> aux theta ta
    | a::ta -> findSterm a theta
      
(* negation of an atom *)
  let neg = function
    | Eq(so,a,b)  -> NEq(so,a,b)
    | NEq(so,a,b) -> Eq(so,a,b)
    | _ -> assert false

(* add a value to the equivalence classes (map) and 
apply the substitutions met since the beginning *)
  let rec add r map sub =
    let map',l' = 
      List.fold_right 
        (fun ((p,q),_) (m,l) -> 
	  let r' = VtoV.find r m in
	  let r'' = subst p q r' in
	  try
	    let _ = VtoV.find r'' m in
	  (*print_value r; print_value r'; print_value r''; print_newline();*)
	    VtoV.add r r'' m, l
	  with Not_found ->
	    VtoV.add r r'' m, r'::l)
        sub 
        (VtoV.add r r map,[])
    in
    addL map' sub l'
      
  and addL map sub = function
    | [] -> map
    | r::tr -> let m = add r map sub in
	       addL m sub tr

(* add a value to the trees of equivalence classes and
apply the substituions met since the beginning *)
  let addUF r u sub =
    let u0,r0 = List.fold_right 
      (fun ((p,q),i) (uf,r') ->
        let r'' = subst p q r' in
        U.addLink uf r' r'' i,r'') 
      sub
      (U.add u r,r)
    in
    u0

  (* delete double apparitions in a list *)
  let noDbl l = 
    let rec aux li = function
    | [] -> li
    | a::ta when List.mem a ta -> aux li ta
    | a::ta -> aux (a::li) ta
    in aux [] l

  let union l l' = noDbl (List.append l l')

  (* explain an equality a=b, given the equivalence trees *)
  let rec explain u a b =
    explPath u (U.explain u (make a) (make b))

  and explCongr u a b = 
    let rec aux l = function
      | [],[] -> l
      | ah::atl, bh::btl -> aux (union l (explain u ah bh)) (atl,btl)
      | _,_ -> assert false
    in
    aux [] (directSubterms a, directSubterms b)

  (* get the explaination of the congruences on the path *)
  and explPath u l =
    let rec aux r = function
      | []    -> r
      | (Eq(_,a,b) as h)::tl when List.mem h r -> aux r tl
      | (Eq(_,a,b) as h)::tl                   -> aux (h::r) tl
      | Congr(a,b)::tl -> aux (union r (explCongr u a b)) tl
      | NEq(_,_,_)::_ -> assert false
    in aux [] l

  let normalise s t = VtoV.find (make t) s.delta

  let get r g = try VtoTSet.find r g with Not_found -> TSet.empty

  (* compute a step of the algorithm *)
  let step s phi i =
    match i with
    | Eq(_,a,b) | Congr(a,b) when (TSet.mem a s.theta) && (TSet.mem b s.theta) -> begin
      let ra = VtoV.find (make a) s.delta in
      let rb = VtoV.find (make b) s.delta in
      if X.vequal ra rb
      then (* rule REMOVE *)
      (*print_string "remove "; (*print_term a; print_term b;*) print_newline();*)
        s,phi
      else
	match solve ra rb with
	(* rule UNSOLV : the explanations are those of all the modifications of the value of the representatives of a and b, plus a=b *)
	| Bot -> (*print_string "unsolv "; (*print_term a; print_term b;*) print_newline();*)
	   let l  = union (U.explain s.u ra (make a)) (U.explain s.u rb (make b)) in
	   let l' = explPath s.u (i::l) in 
	   raise (Inconsistency l')
	| Top -> assert false
	| Sol(p,q) ->
	   (*print_value p; print_value q; print_newline();*)
	   (* we get a substitution and apply it on the equivalence classes *)
	   let delta0 = VtoV.map (subst p q) s.delta in 
	   let delta' = VtoV.fold 
             (fun r d -> 
	       let r' = VtoV.find r delta0 in 
               begin try 
	               let _ = VtoV.find r' d
                       in d 
	         with Not_found -> add r' d (((p,q),i)::s.sub)
	       end)
             delta0
             delta0
           in
	   let u0 = U.addLink s.u (make a) (make b) i in
	   let u' = VtoV.fold 
             (fun r uf ->
	       begin try 
		       let r'  = VtoV.find r s.delta in
		       let r'' = VtoV.find r delta' in
		       if X.vequal r' r'' then uf
                       else
		         try
			   if X.vequal (U.find uf r') (U.find uf r'')
                           then uf
                           else
		             (*print_value r; print_value r'; print_value r''; print_newline();*)
			     U.addLink uf r' r'' i
		         with Not_found -> (* r'' unknown in uf *)
			   let uf' = addUF r'' uf (((p,q),i)::s.sub) in
			   U.addLink uf' r' r'' i
		       (* if r' <> r'' then *)
		       (*   try *)
		       (*     if U.find uf r' <> U.find uf r'' *)
                       (*     then *)
		       (*    (\*print_value r; print_value r'; print_value r''; print_newline();*\) *)
		       (*       U.addLink uf r' r'' i *)
		       (*     else uf *)
		       (*   with Not_found -> (\* r'' unknown in uf *\) *)
		       (*     let uf' = addUF r'' uf (((p,q),i)::s.sub) in *)
		       (*     U.addLink uf' r' r'' i *)
		       (* else uf *)
	         with Not_found -> (* r unknown in s.delta *)
		   addUF r uf (((p,q),i)::s.sub)
	       end)
             delta'
             u0
           in
	  begin
	    match coherent delta' s.n with
	      (* if the substitution isn't coherent with the disequalities
		 we apply the rule INCOHEQ *)
	    | Some(so,a,b) -> (*print_string "incoheq "; (*print_term a; print_term b;*) print_newline();*)
	      raise (Inconsistency(NEq(so,a,b)::(explain u' a b)))
	    (* else this is the rule CONGR *)
	    | None -> (*print_string "congr "; (*print_term a; print_term b;*) print_newline();*)
	      (* we compute the new gamma *)
	      let gamma' = VSet.fold
                (fun l g -> VtoTSet.add l (TSet.union (get l s.gamma) (get p s.gamma)) g) 
		(leaves q)
                VtoTSet.empty
              in
	      (* su is the domain on which we will search the new congruences *)
	      let su = TSet.fold 
                (fun t su -> 
		  if VSet.mem p (leaves (VtoV.find (make t) s.delta)) 
                  then
		    let ssu = VSet.fold
                      (fun l ssu -> TSet.inter ssu (get l s.gamma))
		      (leaves (VtoV.find (make t) delta'))
                      (s.theta)
                    in 
		    TSet.union su ssu 
		else su)
                s.theta
                TSet.empty
              in
	      (* those are the new congruences *)
	      let phi' = TSet.fold
                (fun t l -> 
		  TSet.fold 
                    (fun u li -> 
		      if (root t = root u)&&
		        (equal (directSubterms t) (directSubterms u) delta') 
		      then (Congr(t,u))::li 
		      else li)
                    (TSet.union (get p s.gamma) su)
                    l) 
		(get p s.gamma)
                []
              in
	      (* this is the new state *)
	      {theta = s.theta; 
               gamma = VtoTSet.union s.gamma gamma'; 
	       delta = delta';
               n   = s.n ; 
               sub = ((p,q),i)::s.sub;
               u   = u'},
              List.append phi' phi
	  end
    end
    | NEq(so,a,b) when (TSet.mem a s.theta)&&(TSet.mem b s.theta) ->
      if X.vequal (VtoV.find (make a) s.delta) (VtoV.find (make b) s.delta)
      then
	(* rule INCOHDIFF *)
	(*print_string "incohdiff "; (*print_term a; print_term b;*) print_newline();*)
	raise (Inconsistency(i::(explain s.u a b)))
      else 
	(* rule DIFF *)
	(*print_string "diff "; (*print_term a; print_term b;*) print_newline();*)
        {s with n = (so,a,b)::s.n}, phi
	(* rule ADD *)
    | Eq(_,a,b) | NEq(_,a,b) | Congr(a,b) ->
      (* we find a good subterm to add *)
      let fs = findSterm (if not (TSet.mem a s.theta) then a else b) s.theta in
      (*print_string "add "; (*print_term fs*); print_newline();*)
      (* this is the L_{delta} of the algorithm *)
      let ld = List.fold_left
        (fun e v -> 
	  VSet.union e (leaves (VtoV.find (make v) s.delta)))
	VSet.empty
        (directSubterms fs)
      in
      let gamma' = VSet.fold
        (fun l g -> 
	  VtoTSet.add l (TSet.union (get l s.gamma) (TSet.add fs TSet.empty)) g)
        ld
        VtoTSet.empty in
      (* these are the new congruences *)
      let phi' = TSet.fold 
        (fun t l -> 
	  if (root fs = root t)&&
	    (equal (directSubterms fs) (directSubterms t) s.delta)
          then (Congr(fs,t))::l 
	  else l)
	(VSet.fold (fun l e -> TSet.inter e (get l s.gamma)) ld (s.theta))
        [] 
      in
      (* we add a new equivalence class if necessary *)
      let delta',u' = 
        try
	  let _ = VtoV.find (make fs) s.delta in
	  s.delta, s.u
	with Not_found -> let u' = addUF (make fs) s.u s.sub in
			  add (make fs) s.delta s.sub, u'
      in
      {theta = TSet.add fs s.theta; 
       gamma = VtoTSet.union gamma' s.gamma;
       delta = delta';
       n     = s.n; 
       sub   = s.sub;
       u     = u' },
      List.append phi' (i::phi)
        
  (* the main algorithm *)
  let rec algo arg = function
    | []     -> arg
    | a::phi -> let s,newphi = step arg phi a in algo s newphi
      
  let init =
    {theta = TSet.empty ;
     gamma = VtoTSet.empty ; 
     delta = VtoV.empty ;
     n   = [] ;
     sub = [];
     u   = U.create }

end
