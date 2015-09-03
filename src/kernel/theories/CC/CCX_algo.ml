(* Implementation of CC(X) *)

open Interfaces

module Algo 
  (X:SolvableTheory)
  (U:PersistentUnionFind with type e = X.v and type d = X.t input) = 
struct
  
  open X

  (* let inputToString = function *)
  (*   | (Eq(a,b)) -> (toString a)^" = "^(toString b) *)
  (*   | (NEq(a,b)) -> (toString a)^" <> "^(toString b) *)
  (*   | (IsEq(a,b)) -> (toString a)^" =? "^(toString b) *)
  (*   | (IsNEq(a,b)) -> (toString a)^" <>? "^(toString b) *)
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
                n     : X.t input list; 
                phi   : X.t input list;
                sub   : ((X.v * X.v) * U.d) list;
                u     : U.t
               }

  exception Inconsistency of X.t input list

  exception Unknown

(* verify if our equivalence classes or coherent with a set of disequalities
   if not : output the 'bad' disequality *)
  let rec coherent delta = function 
    | [] -> None
    | i::t -> begin match i with
      | NEq(a,b) ->
        if VtoV.find (make a) delta = VtoV.find (make b) delta
        then Some(NEq(a,b))
	else coherent delta t
      | _ -> assert false
    end

(* verify if the terms are in the same equivalence class 2 by 2 *)
  let rec equal l l' delta =
    match l,l' with
    | [], [] -> true
    | a::ta, b::tb when VtoV.find (make a) delta = VtoV.find (make b) delta -> equal ta tb delta
    | _ -> false

  exception Finished

(* find a subterm such that it is not in theta but all its direct subterms are *)
  let rec findSterm t theta =
    try aux theta (directSubterms t) with Finished -> t
  and aux theta = function
    | [] -> raise Finished
    | a::ta when TSet.mem a theta -> aux theta ta
    | a::ta -> findSterm a theta
      
(* transform an atom in a query *)
  let toQuery = function 
    | Eq(a,b)  -> IsEq(a,b)
    | NEq(a,b) -> IsNEq(a,b)
    | _ -> assert false

(* complementary operation *)      
  let fromQuery = function
    | IsEq(a,b)  -> Eq(a,b)
    | IsNEq(a,b) -> NEq(a,b)
    | _ -> assert false

(* negation of an atom *)
  let neg = function
    | Eq(a,b)  -> NEq(a,b)
    | NEq(a,b) -> Eq(a,b)
    | _ -> assert false

(* add a value to the equivalence classes (map) and 
apply the substitutions met since the beginning *)
  let rec add r map sub =
    let l = ref [] in
    let map' = 
      List.fold_right 
        (fun ((p,q),_) m -> 
	  let r' = VtoV.find r m in
	  let r'' = subst p q r' in
	  try
	    let _ = VtoV.find r'' m in
	  (*print_value r; print_value r'; print_value r''; print_newline();*)
	    VtoV.add r r'' m
	  with Not_found ->
	    l := r''::!l;
	    VtoV.add r r'' m)
        sub 
        (VtoV.add r r map)
    in
    addL map' sub !l
      
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

  (* explain an atom (equality), given the equivalence trees *)
  let rec explain t u =
    (*print_string "explain "; print_input t; print_string ": ";*)
    match t with
    | Eq(a,b) -> let ra = make a in
		 let rb = make b in
		   (* we compute the first common ancestor *)
		 let r = U.fca u ra rb in 
		   (* and the explaination on the paths to it *)
		   (*print_value ra; print_value rb; print_value r;*)
		 let pa = U.pathTo u ra r in
		 let pb = U.pathTo u rb r in
		   (* and we have to explain the congruences on these paths *)
		 let l = explPath (union pa pb) u in
		   (*print_inputlist l; print_newline();*)
		 l
    (* if we have a congruence, we need to explain the equalities of the direct subterms 2 by 2 *)
    | Congr(a,b) ->
      let rec aux l = function
        | [],[] -> l
        | ah::atl, bh::btl -> aux (union l (explain (Eq(ah,bh)) u)) (atl,btl)
        | _,_ -> assert false
      in
      aux [] (directSubterms a, directSubterms b)

    | _ -> assert false
      
(* get the explaination of the congruences on the path *)
  and explPath l u =
    let rec aux r = function
      | []    -> r
      | Eq(a,b)::tl when List.mem (Eq(a,b)) r -> aux r tl
      | Eq(a,b)::tl    -> aux ((Eq(a,b))::r) tl
      | Congr(a,b)::tl -> aux (union r (explain (Congr(a,b)) u)) tl
      | _ -> assert false
    in aux [] l


  let get r g = try VtoTSet.find r g with Not_found -> TSet.empty

(* compute a step of the algorithm *)
  let rec step s =
    match List.hd s.phi with
    | Eq(a,b) | Congr(a,b) as i when 
	(TSet.mem a s.theta)
        &&(TSet.mem b s.theta)
        &&((VtoV.find (make a) s.delta)<>(VtoV.find (make b) s.delta)) -> 
      begin
	match solve (VtoV.find (make a) s.delta) (VtoV.find (make b) s.delta) with
	  (* rule UNSOLV : the explainations are those of all the modifications of the value of the representatives of a and b, plus a=b *)
	| Bot -> (*print_string "unsolv "; (*print_term a; print_term b;*) print_newline();*)
	  let l  = union (U.path s.u (make a)) (U.path s.u (make b)) in
	  let l' = explPath l s.u in 
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
		      if r'<>r'' then
		      try
			if U.find uf r' <> U.find uf r''
                        then
		          (*print_value r; print_value r'; print_value r''; print_newline();*)
			  U.addLink uf r' r'' i
			else uf
		      with Not_found -> (* r'' unknown in uf *)
			let uf' = addUF r'' uf (((p,q),i)::s.sub) in
			U.addLink uf' r' r'' i
		      else uf
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
	    | Some t -> (*print_string "incoheq "; (*print_term a; print_term b;*) print_newline();*)
	      raise (Inconsistency(t::(explain (neg t) u')))
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
	       phi = List.append phi' (List.tl s.phi);
               sub = ((p,q),i)::s.sub;
               u   = u'}
	  end
      end
	(* rule REMOVE *)
    | Eq(a,b) | Congr(a,b) when 
	(TSet.mem a s.theta)&&(TSet.mem b s.theta)&&
	  (VtoV.find (make a) s.delta = VtoV.find (make b) s.delta) -> 
      (*print_string "remove "; (*print_term a; print_term b;*) print_newline();*)
      {s with phi = List.tl s.phi}
    | NEq(a,b) when (TSet.mem a s.theta)&&(TSet.mem b s.theta) ->
      if VtoV.find (make a) s.delta = VtoV.find (make b) s.delta then
	(* rule INCOHDIFF *)
	(*print_string "incohdiff "; (*print_term a; print_term b;*) print_newline();*)
	raise (Inconsistency((NEq(a,b))::(explain (Eq(a,b)) s.u)))
      else 
	(* rule DIFF *)
	(*print_string "diff "; (*print_term a; print_term b;*) print_newline();*)
        {s with n = (NEq(a,b))::(NEq(b,a))::s.n; phi = List.tl s.phi}
    | IsEq(a,b) when (TSet.mem a s.theta)&&(TSet.mem b s.theta) ->
      if VtoV.find (make a) s.delta = VtoV.find (make b) s.delta then
	(* rule QUERY *)
	(*print_string "query "; (*print_term a; print_term b;*) print_newline();*)
	{s with phi = List.tl s.phi}
      else 
	(* the equality isn't necessarily a consequence 
	   of the other litterals *)
	(*print_string "bad query "; (*print_term a; print_term b;*) print_newline();*)
	 raise Unknown
    | IsNEq(a,b) when (TSet.mem a s.theta)&&(TSet.mem b s.theta) ->
      if VtoV.find (make a) s.delta = VtoV.find (make b) s.delta then
	(* the disequality isn't necessarily a consequence 
	   of the other litteral *)
	(*print_string "bad querydiff: equal terms "; (*print_term a; print_term b;*) print_newline();*)
	 raise Unknown
      else begin try
	     let _ = algo {s with phi = [Eq(a,b)]}
	     in (* the disequality isn't necessarily a consequence 
	   of the other litterals *)
	     (*print_string "bad querydiff: non necessarily disequal terms "; (*print_term a; print_term b;*) print_newline();*)
	     raise Unknown
	with Inconsistency l -> (* rule QUERYDIFF *) 
	  (*print_string "querydiff "; (*print_term a; print_term b;*) print_newline();*)
	  U.clear();
	  {s with phi = List.tl s.phi}
      end
	(* rule ADD *)
    | Eq(a,b) | NEq(a,b) | IsEq(a,b) | IsNEq(a,b) | Congr(a,b) ->
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
       phi   = List.append phi' s.phi;
       sub   = s.sub;
       u     = u' }
        
(* the algorithm without conflict analysis *)
  and algo arg =
    match arg.phi with
    | [] -> arg
    | _  -> algo (step arg)
      
(* remove an atom of a list *)
  let remove t = 
    let rec aux li = function
      | []    -> li
      | a::ta when a = t -> aux li ta
      | a::ta -> aux (a::li) ta
    in aux []

(* the conflict analysis *)
  let analyse s = function
    | Eq(a,b) -> explain (Eq(a,b)) s.u
      (* a disequality is explained by the litterals that make 
	 the equality inconsistent *)
    | NEq(a,b) -> 
      begin try
	      let _ = algo { s with phi = [Eq(a,b)] } in
	      assert false
        with
        | Inconsistency l -> remove (Eq(a,b)) l
        | _ -> assert false
      end
    | _ -> assert false


  let normalise s t = VtoV.find (make t) s.delta

  let init =
    {theta = TSet.empty ;
     gamma = VtoTSet.empty ; 
     delta = VtoV.empty ;
     n   = [] ;
     phi = [];
     sub = [];
     u   = U.create }

(* the function to call to solve a problem :
   given a list of atoms (in their input form) and an optional atom,
   check if the atom is entailed by the list, 
   or if the list is consistent with the theory *)
  let solve phi = function
    | None -> (*print_string "\nConsistency\n";*)
      (*print_inputlist phi; print_newline();*)
      begin
	try
	  let _ = algo {init with phi = phi} in
	  U.clear();
	  None
	with 
	| Unknown -> assert false
	| Inconsistency l ->
	  (*print_inputlist l; print_newline();*)
	  U.clear();
	  Some l
      end
    | Some q -> (*print_string "\nGoal_consistency: "; print_inputlist q; print_newline();*)
      (*print_inputlist phi; print_newline();*)
      begin
	try
	  let s' = algo { init with phi = List.append phi q } in
	  let l = List.fold_left 
            (fun l e -> union l (analyse s' (fromQuery e)))
            []
            q
          in
	  (*print_inputlist l; print_newline();*)
	  U.clear();
	  Some l
	with 
	| Inconsistency _ | Unknown -> U.clear(); None
      end 

end
