open Format

open Kernel
open Interfaces_I
open Formulae
open ThDecProc_tools
open MyAtom
open Lib.Patricia
open Lib.SetConstructions

module Sig    = ThSig_register.FOSig

let sugPlugin = None

module IAtom  = struct

  module Atom   = MyAtom.Atom(struct
    type t = int
    let id i = i
    let print_in_fmt fmt i = fprintf fmt "%i" i
    let clear () = ()
    let compare = Pervasives.compare
  end)

  module DSubst = StandardDSubst

  module A = MyAtom.Atom(struct
    type t = DSubst.freeVar
    let id = function
      | DSubst.Eigen i -> 2*(i+1)
      | DSubst.Meta i  -> 2*(i+1)+1

    let print_in_fmt fmt = function
      | DSubst.Eigen i -> fprintf fmt "%i" i
      | DSubst.Meta i  -> fprintf fmt "?%i" i

    let clear () = ()
    let compare v v' = Pervasives.compare (id v) (id v')
  end)

  type t = { substituted: A.t; original: Atom.t*DSubst.t}

  let id s = A.id s.substituted

  let expose a = A.reveal a.substituted

  let print_in_fmt fmt s = A.print_in_fmt fmt s.substituted

  let clear = A.clear
  let compare v v' = A.compare v.substituted v'.substituted
  let reveal a = a.original


  let build (a,d) = 
    let rec propagate t = match Atom.Term.reveal t with
      | Atom.Term.V i     -> A.Term.bV(DSubst.get i d)
      | Atom.Term.C(f,tl) -> A.Term.bC f (List.map propagate tl)
    in
    let (b,p,tl) = Atom.reveal a in
    { substituted = A.build (b,p,List.map propagate tl);
      original = (a,d)}

end

module Term  = IAtom.A.Term
module D     = IAtom.DSubst
module Arity = D.Arity

module Do = struct
  type keys = int
  let kcompare = Pervasives.compare
  type values = Term.t
  let vcompare t1 t2 = Pervasives.compare (Term.id t1) (Term.id t2)
  type infos = keys m_infos
  let info_build = m_info_build kcompare
  let treeHCons = true
end

module UMap = PATMap(Do)(TypesFromHConsed(struct 
  type t = int 
  let id i = i
end))

module Unification =
struct

  exception WrongArgumentNumber
  exception NonUnifiable

  open Term

  let rec expose ((n,v) as u) t = match reveal t with
    | V(D.Meta i) when UMap.mem i v -> expose u (UMap.find i v)
    | a -> a

  let trans get_meta2 get_key1 loc21 i2 =
    if Arity.IntMap.mem i2 get_meta2
    then Some(Arity.IntMap.find (Arity.IntMap.find i2 get_meta2) get_key1)
    else if Arity.IntMap.mem i2 loc21
    then Some(Arity.IntMap.find i2 loc21)
    else None

  let occurs_check fold a u t get_key =
    let rec aux t = match expose u t with
      | V(D.Eigen i) -> fold i (fun mv -> aux (bV(D.Meta (Arity.IntMap.find mv get_key))))
      | C(a,l)       -> List.fold_left (fun () -> aux) () l
      | V(D.Meta i) when i!=a -> ()
      | V(D.Meta i)  -> raise NonUnifiable
    in
    aux t

  let translate b fold a0 u2 t2 get_meta2 get_key1 cont =
    let rec aux cont t2 = match expose u2 t2 with
      | V(D.Eigen i as ei) when b -> fun u1 ->
        fold i (fun mv -> occurs_check fold a0 u1 (bV(D.Meta (Arity.IntMap.find mv get_key1))) get_key1);
        cont (bV ei) u1
      | C(a,l) when b ->
        let rec aux_tl cont = function
          | []   -> cont []
          | h::l -> let newcont1 h' =
                      let newcont2 l' =
                        cont (h'::l') 
                      in aux_tl newcont2 l
                    in aux newcont1 h
        in aux_tl (fun l' -> cont (bC a l')) l
      | V(D.Meta i2) -> 
        fun ((next,v1) as u1) loc12 loc21 ->
          begin
            match trans get_meta2 get_key1 loc21 i2 with
            | None ->  
              let u1' = (next+1,v1) in
              let loc12' = Arity.IntMap.add next i2 loc12 in
              let loc21' = Arity.IntMap.add i2 next loc21 in
              cont (bV(D.Meta next)) u1' loc12' loc21'
            | Some i1 when b && i1!=a0 -> cont (bV(D.Meta i1)) u1 loc12 loc21
            | Some _ -> raise NonUnifiable
          end
      | _ -> raise NonUnifiable
    in
    let newcont t1 (next,v1) = 
      let v1' = UMap.add a0 (fun _ -> t1) v1 in
      cont (next,v1') u2
    in
    aux newcont t2

  let rec combine l = function
    | [],[]             -> l
    | (a1::l1),(a2::l2) -> combine ((a1,a2)::l) (l1,l2)
    | _,_               -> raise WrongArgumentNumber

  let unif b fold get_key1 get_meta1 get_key2 get_meta2 u1 u2 l =
    let rec aux l u1 u2 loc12 loc21 = match l with
      | []         -> Some(u1,u2)
      | (t1,t2)::l -> 
        try match expose u1 t1,expose u2 t2 with

        | (V(D.Eigen a),V(D.Eigen b)) when a==b -> aux l u1 u2 loc12 loc21

        | (C(a,l1),C(b,l2)) -> aux (combine l (l1,l2)) u1 u2 loc12 loc21

        | (V(D.Meta a),V(D.Meta b)) when 
            (match trans get_meta2 get_key1 loc21 b with
            | Some i -> i==a
            | None   -> false)
            -> aux l u1 u2 loc12 loc21
          
        | (V(D.Meta a),_)  -> translate b fold a u2 t2 get_meta2 get_key1 (aux l) u1 loc12 loc21

        | (_,V(D.Meta a))  -> translate b fold a u1 t1 get_meta1 get_key2 (fun u2' u1' loc21' loc12' -> aux l u1' u2' loc12' loc21') u1 loc12 loc21

        | (_,_)            -> raise NonUnifiable
        with NonUnifiable -> None
    in
    aux l u1 u2 Arity.IntMap.empty Arity.IntMap.empty

end


module Constraint = struct

  type t = { ar : Arity.t;
             get_key : int Arity.IntMap.t;
             get_meta: int Arity.IntMap.t;
             next_key: int;
             unifier : UMap.t}

  let topconstraint = { ar  = Arity.init;
                        get_key  = Arity.IntMap.empty;
                        get_meta = Arity.IntMap.empty;
                        next_key = 0;
                        unifier  = UMap.empty}

  let liftE u =  {
    ar       = Arity.liftE u.ar;
    get_key  = u.get_key;
    get_meta = u.get_meta;
    next_key = u.next_key;
    unifier  = u.unifier
  }

  let projE u = {
    ar       = Arity.projE u.ar;
    get_key  = u.get_key;
    get_meta = u.get_meta;
    next_key = u.next_key;
    unifier  = u.unifier
  }

  let liftM u =  {
    ar       = Arity.liftM u.ar;
    get_key  = Arity.IntMap.add u.ar.Arity.next_meta u.next_key u.get_key;
    get_meta = Arity.IntMap.add u.next_key u.ar.Arity.next_meta u.get_meta;
    next_key = u.next_key+1;
    unifier  = u.unifier
  }

  let projM u = {
    ar       = Arity.projM u.ar;
    get_key  = Arity.IntMap.remove (u.ar.Arity.next_meta-1) u.get_key;
    get_meta = Arity.IntMap.remove (Arity.IntMap.find (u.ar.Arity.next_meta-1) u.get_key) u.get_meta;
    next_key = u.next_key;
    unifier  = u.unifier
  }

  let liftAr u ar =
    let diff = ar.Arity.next_meta - u.ar.Arity.next_meta in
    let rec liftN ui = function
      | 0 -> ui
      | n -> liftN (liftM ui) (n-1)
    in
    let tmp = liftN u diff in
    { ar  = ar;
      get_key  = tmp.get_key;
      get_meta = tmp.get_meta;
      next_key = tmp.next_key;
      unifier  = tmp.unifier}


  let unif_aux b sigma1 sigma2 l1 l2 =
    if not(Arity.prefix sigma1.ar sigma2.ar)
    then None
    else
      let sigma1 =  liftAr sigma1 sigma2.ar in
      let f sigma = Term.subst 
        (function
        | D.Meta i -> D.Meta (Arity.IntMap.find i sigma1.get_key)
        | a -> a)
      in
      match
        Unification.unif 
          b
          (fun i f ->
            for j = 0 to (Arity.IntMap.find i sigma2.ar.Arity.eigen_dependencies) - 1 do
              f j
            done)
          sigma1.get_key 
          sigma1.get_meta 
          sigma2.get_key 
          sigma2.get_meta 
          (sigma1.next_key,sigma1.unifier) 
          (sigma2.next_key,sigma2.unifier)
          (Unification.combine [] (List.map (f sigma1) l1, List.map (f sigma2) l2))
      with
      | None          -> None
      | Some((n,u),_) -> Some {
        ar = sigma1.ar;
        get_key  = sigma1.get_key;
        get_meta = sigma1.get_meta;
        next_key = n;
        unifier  = u
      }

  let unif = unif_aux true

  let meet_aux b sigma1 sigma2 = 
    let l1 = 
      Arity.IntMap.fold
        (fun _ key l -> Term.bV(D.Meta key)::l)
        sigma1.get_key
        []
    in
    let l2 = 
      Arity.IntMap.fold
        (fun _ key l -> Term.bV(D.Meta key)::l)
        sigma2.get_key
        []
    in
    unif_aux b sigma1 sigma2 l1 l2

  let meet = meet_aux true

  let compare sigma1 sigma2 =
    match meet_aux false sigma1 sigma2 with 
    | Some _ -> 0
    | None   -> 1

end


module Consistency(ASet: CollectImplem with type e = IAtom.t)
  = struct    

    exception FoundIt of ASet.t*Constraint.t*(ASet.t,Constraint.t) stream

    (* We range over atomN,
       alias is the set of atoms that haven't been tried (starts with atomN);
       as soon as we find a solution, we raise an exception *)

    let aux aset compare cont t atomN sigma = 
      ASet.fold
        (fun a alias -> 
          let newalias = ASet.remove a alias in
          let (b1,p1,l1) = IAtom.expose a in
          let (b2,p2,l2) = IAtom.expose t in
          (if (compare b1 b2) && (Predicates.compare p1 p2 == 0)
           then match Constraint.unif sigma sigma l1 l2 with
           | Some newsigma -> 
             raise (FoundIt(ASet.add a aset,newsigma, cont newalias))
           | None -> ());
          newalias)
        atomN 
        atomN 
        
    let rec goal_consistency t atomN sigma = 
      (* We range over atomN,
         we catch an exception (means success);
         otherwise we finish with NoMore *)
      try 
        let _ = 
          aux ASet.empty (=) (goal_consistency t) t atomN sigma
        in NoMore
      with
        FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)
          
    let rec consistency atomN sigma =
      try 
        let _ =
          ASet.fold
            (fun t after -> 
              let newafter = ASet.remove t after in
              let aset = ASet.add t ASet.empty in
              let rec g_consistency remaining sigma =
                try 
                  let _ = aux aset (!=) g_consistency t remaining sigma in
                  consistency newafter sigma
                with
                  FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)
              in
              let _ = aux aset (!=) g_consistency t newafter sigma in newafter
            )
            atomN 
            atomN 
        in
        NoMore
      with
        FoundIt(a,newsigma,f) -> Guard(a,newsigma,f)

  end



module Structure(F:FormulaType with type lit = IAtom.Atom.t)
  = struct

    module PS = ThDecProc_tools.PropStructure(F)

    open Theories

    module BTerm = IAtom.Atom.Term

    type t = Term of BTerm.t | Prop of F.t

    let lit (b, f, tl) = F.lit(IAtom.Atom.bbuild (b, f, tl))

    let toform = function
      | Prop f -> f
      | _      -> raise (ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

    let toterm = function
      | Term t -> t
      | _ -> raise (ModelError "ModelError: trying to convert into a term an expression that clearly is not one")

    let st = 
      { sigsymb_i = 
          (fun s l -> Prop (PS.symb_i s (List.map toform l))
	  );
	decsymb_i =
          (function
	  | `Prop -> fun (var:string) l -> 
	    Prop (lit (true,var,List.map toterm l))
	  | `Term -> fun (var:string) l -> 
	    Term (BTerm.bC var (List.map toterm l))
	  | _     -> fun (var:string) -> raise (ModelError ("ModelError: variable "^var^" not of expected type `Prop or `Term")));
        boundsymb_i = (fun db so -> Term (BTerm.bV db));
        quantif_i   = (fun b so sf -> 
          let f = toform sf in
          Prop ((if b then F.forall else F.exists) f))
      }

    let	examples = []

  end
