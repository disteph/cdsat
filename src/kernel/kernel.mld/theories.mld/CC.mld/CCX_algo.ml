(* Implementation of CC(X) *)

open Top
open Specs

open Interfaces

module Algo 
  (DS: GlobalDS)
  (X:SolvableTheory with type VtoAssign.v = DS.Assign.t
                    and  type t = DS.Term.t)
  (U:PersistentUnionFind with type e = X.v) = 
struct

  let directSubterms t = 
    match Terms.reveal t with
    | Terms.C(f,l) -> l
    | Terms.V _
      | Terms.FB(_,_,_) -> []

  let root t = 
    match Terms.reveal t with
    | Terms.C(f,l) -> Some f
    | Terms.V _
      | Terms.FB(_,_,_) -> None
  
  open X
  open DS

  type state = {theta : Assign.t;
                gamma : VtoAssign.t;
                delta : VtoV.t; 
                n     : (Sorts.t*Term.t*Term.t*(int option)) list;
                sub   : ((X.v * X.v) * Term.t input) list;
                u     : Term.t input U.t
               }

  exception Inconsistency of Term.t input list

(* verify if our equivalence classes are coherent with a set of disequalities
   if not : output the 'bad' disequality *)
  let rec coherent delta = function 
    | [] -> None
    | ((_,a,b,_) as h)::t
        when X.vequal (VtoV.find (make a) delta) (VtoV.find (make b) delta)
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
    | a::ta when Assign.mem a theta -> aux theta ta
    | a::ta -> findSterm a theta
      
(* add a value to the equivalence classes (map) and 
apply the substitutions met since the beginning *)
  let rec add r map sub =
    let map',l' = 
      List.fold
        (fun ((p,q),_) (m,l) -> 
	  let r' = VtoV.find r m in
	  let r'' = subst p q r' in
	  try
	    let _ = VtoV.find r'' m in
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
    let u0,r0 = List.fold
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
    | a::ta when List.mem (equal_input Term.equal) a ta -> aux li ta
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
      | (Eq(_,_,_,_) as h)::tl when List.mem (equal_input Term.equal) h r -> aux r tl
      | (Eq(_,_,_,_) as h)::tl                   -> aux (h::r) tl
      | Congr(a,b)::tl -> aux (union r (explCongr u a b)) tl
      | NEq(_,_,_,_)::_ -> assert false
    in aux [] l

  let normalise s t = VtoV.find (make t) s.delta

  let get r g = try VtoAssign.find r g with Not_found -> Assign.empty

  (* compute a step of the algorithm *)
  let step s phi i =
    match i with
    | Eq(_,a,b,_) | Congr(a,b) when (Assign.mem a s.theta) && (Assign.mem b s.theta) -> begin
      let ra = VtoV.find (make a) s.delta in
      let rb = VtoV.find (make b) s.delta in
      if X.vequal ra rb
      then (* rule REMOVE *)
        s,phi
      else
	match solve ra rb with
	(* rule UNSOLV : the explanations are those of all the modifications of the value of the representatives of a and b, plus a=b *)
	| Bot ->
	   let l  = union (U.explain s.u ra (make a)) (U.explain s.u rb (make b)) in
	   let l' = explPath s.u (i::l) in 
	   raise (Inconsistency l')
	| Top -> assert false
	| Sol(p,q) ->
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
	    | Some(so,a,b,tag) ->
	       raise (Inconsistency(NEq(so,a,b,tag)::(explain u' a b)))
	    (* else this is the rule CONGR *)
	    | None ->
	      (* we compute the new gamma *)
	      let gamma' = VSet.fold
                (fun l g -> VtoAssign.add l (Assign.union (get l s.gamma) (get p s.gamma)) g) 
		(leaves q)
                VtoAssign.empty
              in
	      (* su is the domain on which we will search the new congruences *)
	      let su = Assign.fold 
                (fun t su -> 
		  if VSet.mem p (leaves (VtoV.find (make t) s.delta)) 
                  then
		    let ssu = VSet.fold
                      (fun l ssu -> Assign.inter ssu (get l s.gamma))
		      (leaves (VtoV.find (make t) delta'))
                      (s.theta)
                    in 
		    Assign.union su ssu 
		else su)
                s.theta
                Assign.empty
              in
	      (* those are the new congruences *)
	      let phi' = Assign.fold
                (fun t l -> 
		  Assign.fold 
                    (fun u li -> 
		      if ([%eq: Symbols.t option] (root t) (root u))&&
		        (equal (directSubterms t) (directSubterms u) delta') 
		      then (Congr(t,u))::li 
		      else li)
                    (Assign.union (get p s.gamma) su)
                    l) 
		(get p s.gamma)
                []
              in
	      (* this is the new state *)
	      {theta = s.theta; 
               gamma = VtoAssign.union s.gamma gamma'; 
	       delta = delta';
               n   = s.n ; 
               sub = ((p,q),i)::s.sub;
               u   = u'},
              List.append phi' phi
	  end
    end
    | NEq(so,a,b,tag) when (Assign.mem a s.theta)&&(Assign.mem b s.theta) ->
      if X.vequal (VtoV.find (make a) s.delta) (VtoV.find (make b) s.delta)
      then
	(* rule INCOHDIFF *)
	raise (Inconsistency(i::(explain s.u a b)))
      else 
	(* rule DIFF *)
        {s with n = (so,a,b,tag)::s.n}, phi
    (* rule ADD *)
    | Eq(_,a,b,_) | NEq(_,a,b,_) | Congr(a,b) ->
      (* we find a good subterm to add *)
      let fs = findSterm (if not (Assign.mem a s.theta) then a else b) s.theta in
      (* this is the L_{delta} of the algorithm *)
      let ld = List.fold
        (fun v e -> 
	  VSet.union e (leaves (VtoV.find (make v) s.delta)))
        (directSubterms fs)
	VSet.empty
      in
      let gamma' = VSet.fold
        (fun l g -> 
	  VtoAssign.add l (Assign.union (get l s.gamma) (Assign.add fs Assign.empty)) g)
        ld
        VtoAssign.empty in
      (* these are the new congruences *)
      let phi' = Assign.fold 
        (fun t l -> 
	  if ([%eq: Symbols.t option] (root fs) (root t)) &&
	    (equal (directSubterms fs) (directSubterms t) s.delta)
          then (Congr(fs,t))::l 
	  else l)
	(VSet.fold (fun l e -> Assign.inter e (get l s.gamma)) ld (s.theta))
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
      {theta = Assign.add fs s.theta; 
       gamma = VtoAssign.union gamma' s.gamma;
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
    {theta = Assign.empty ;
     gamma = VtoAssign.empty ; 
     delta = VtoV.empty ;
     n   = [] ;
     sub = [];
     u   = U.create }

end
