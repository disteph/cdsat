open Format

open Kernel
open Interfaces_I
open Formulae
open Tools.AritiesDSubst
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

module IU = (struct

  exception AlreadyConstrained

  module UMap = PATMap(Do)(TypesFromHConsed(struct 
    type t = int 
    let id i = i
  end))

  type t = {next_key:int; map:UMap.t}

  let empty = {next_key = 0; map = UMap.empty}

  type keys   = int
  type values = Term.t
  type eOk    = Eigen of Arity.eigen | Meta of keys

  let reveal t = match Term.reveal t with
    | Term.V(D.Eigen i) -> Term.V(Eigen i)
    | Term.V(D.Meta i)  -> Term.V(Meta i)
    | Term.C(f,tl)      -> Term.C(f,tl)

  let bV = function
    | Eigen i -> Term.bV(D.Eigen i)
    | Meta i  -> Term.bV(D.Meta i)

  let bC = Term.bC

  module KMap = Arity.IntMap

  let add key t u =
    match reveal t with
    | Term.V(Meta k) when key=k -> u
    | _ ->  {
      next_key = u.next_key;
      map = UMap.add key (function None -> t | Some _ -> raise AlreadyConstrained) u.map
    }

  let add_new u =
    (u.next_key, {next_key = u.next_key+1; map = u.map})

  let rec normalise ((t,u) as c) = 
    let aux2 = function
      | (Term.V(D.Meta i),u) when UMap.mem i u.map -> 
        let (t,new_u) = normalise (UMap.find i u.map,u) in
        (t, {next_key = new_u.next_key; map = UMap.add i (fun _ -> t) new_u.map})
      | _ -> c
    in aux2 (Term.reveal t,u)

  let expose c0 = 
    let (t,new_u) = normalise c0 in ((reveal t),new_u)

  let get key u =
    if UMap.mem key u.map
    then Some(normalise(UMap.find key u.map,u))
    else None

  let internalise f = Term.subst (function D.Meta i -> D.Meta (f i) | D.Eigen i -> D.Eigen i)

  let print_in_fmt fmt u =
    let aux fmt =
      UMap.fold
        (fun key term () -> fprintf fmt "k%i -> %a; " key Term.print_in_fmt term)
        u.map
        ()
    in
    fprintf fmt "{next=%i; map = %t}" u.next_key aux

  let print_in_fmtV = Term.print_in_fmt
  let print_in_fmtK fmt = Format.fprintf fmt "%i"

end : sig

  exception AlreadyConstrained

  type t
  val empty:t

  type keys
  type values
  type eOk = Eigen of Arity.eigen | Meta of keys
  val bV: eOk -> values
  val bC: Term.fsymb -> (values list) -> values

  module KMap : Map.S with type key = keys

  val add : keys -> values -> t -> t
  val add_new : t -> keys*t
  val expose: values*t -> (eOk,values) Term.term *t
  val get: keys -> t -> (values * t) option
  val internalise: (Arity.eigen -> keys) -> Term.t -> values
  val print_in_fmt: Format.formatter -> t -> unit
  val print_in_fmtV: Format.formatter -> values -> unit
  val print_in_fmtK: Format.formatter -> keys -> unit

end)

module MKcorr = (struct

  type t = {get_key: IU.keys Arity.IntMap.t; get_meta: Arity.meta IU.KMap.t}
  let empty = {get_key = Arity.IntMap.empty; get_meta = IU.KMap.empty}

  let get_key  f mv = Arity.IntMap.find mv f.get_key
  let get_meta f key = IU.KMap.find key f.get_meta
  let is_meta  f key = IU.KMap.mem key f.get_meta

  let add f mv key = {get_key = Arity.IntMap.add mv key f.get_key;
                      get_meta = IU.KMap.add key mv f.get_meta}

  let remove f mv = let key = get_key f mv in
                    {get_key = Arity.IntMap.remove mv f.get_key;
                     get_meta = IU.KMap.remove key f.get_meta}

  let fold f mk = Arity.IntMap.fold f mk.get_key
  let print_in_fmt fmt mk = 
    Arity.IntMap.fold (fun mv key () -> Format.fprintf fmt "?%i -> k%a; " mv IU.print_in_fmtK key) mk.get_key ()

end: sig

  type t
  val empty: t
  val get_key: t -> Arity.meta -> IU.keys
  val get_meta: t -> IU.keys -> Arity.meta
  val is_meta: t -> IU.keys -> bool
  val add: t -> Arity.meta -> IU.keys -> t
  val remove: t -> Arity.meta -> t
  val fold: (int -> IU.keys -> 'b -> 'b) -> t -> 'b -> 'b
  val print_in_fmt: Format.formatter -> t -> unit
end)

module KKcorr = (struct

  type t = {mk1: MKcorr.t;
            mk2: MKcorr.t;
            loc12: IU.keys IU.KMap.t;
            loc21: IU.keys IU.KMap.t}

  let init f1 f2 = {mk1 = f1;
                    mk2 = f2;
                    loc12 = IU.KMap.empty; 
                    loc21 = IU.KMap.empty}

  let swap f = {mk1 = f.mk2;
                mk2 = f.mk1;
                loc12 = f.loc21; 
                loc21 = f.loc12}

  let get12 f key1 = 
    if MKcorr.is_meta f.mk1 key1
    then Some(MKcorr.get_key f.mk2 (MKcorr.get_meta f.mk1 key1))
    else if IU.KMap.mem key1 f.loc12
    then Some(IU.KMap.find key1 f.loc12)
    else None

  let get21 f key2 = 
    let f' = swap f in
    get12 f' key2

  let add f key1 key2 =
    {mk1 = f.mk1;
     mk2 = f.mk2;
     loc12 = IU.KMap.add key1 key2 f.loc12; 
     loc21 = IU.KMap.add key2 key1 f.loc12}
      
end: sig
  type t
  val init: MKcorr.t -> MKcorr.t -> t
  val swap: t -> t
  val get12: t -> IU.keys -> IU.keys option
  val get21: t -> IU.keys -> IU.keys option
  val add: t -> IU.keys -> IU.keys -> t
end
)


module Unification = (struct
    
  exception WrongArgumentNumber
  exception NonUnifiable

  open Term 

  let rec occurs_check_v lax fold key0 c =
    let (t,u) = IU.expose c in
    match t with
    | V(IU.Eigen ei) -> occurs_check_ei fold key0 u ei
    | V(IU.Meta key) -> occurs_check_k  lax fold key0 u key
    | C(_,l)         -> 
      List.fold_left 
        (fun u' t -> occurs_check_v false fold key0 (t,u'))
        u l

  and occurs_check_k lax fold key0 u key =
    if (not lax)&&(key==key0) then raise NonUnifiable
    else
      match IU.get key u with
      | None   -> u
      | Some c -> occurs_check_v lax fold key0 c

  and occurs_check_ei fold key0 u ei =
    fold ei (fun key u'' -> occurs_check_k false fold key0 u'' key) u


  let translate b fold key0 u2 t2 cont =
    Dump.msg (Some(fun p->p "translate k%a -> %a in %a" IU.print_in_fmtK key0 IU.print_in_fmtV t2 IU.print_in_fmt u2)) None None;
    let rec aux lax cont t2 u2 =
      let (t2',u2') = IU.expose(t2,u2) in
      match t2' with
      | V(IU.Eigen ei) when b -> 
        fun u1 ->
          let u1' = 
            Dump.msg (Some(fun p->p "occurs_check_ei on k%a -> Eigen %d" IU.print_in_fmtK key0 ei)) None None;
            occurs_check_ei fold key0 u1 ei in
          cont (IU.bV(IU.Eigen ei)) u1' u2'
      | C(a,l) when b -> 
        fun u1 ->
          let rec aux_tl cont = function
            | []   -> cont []
            | h::l -> let newcont1 h' =
                        let newcont2 l' =
                          cont (h'::l')
                        in aux_tl newcont2 l
                      in aux false newcont1 h
          in aux_tl (fun l' -> cont(IU.bC a l')) l u1 u2'
      | V(IU.Meta i2) -> 
        fun u1 kk_corr ->
          begin
            match KKcorr.get21 kk_corr i2 with
            | None ->  
              let (newkey,u1') = IU.add_new u1 in
              let kk_corr' = KKcorr.add kk_corr newkey i2 in
              cont (IU.bV(IU.Meta newkey)) u1' u2' kk_corr'
            | Some key1 when b && (lax||(key1!=key0)) -> 
              let (t0,u1') = 
                match IU.get key1 u1 with
                | None             -> (IU.bV(IU.Meta key1),u1)
                | Some((t,_) as c) -> (t,occurs_check_v lax fold key0 c)
              in cont t0 u1' u2' kk_corr
            | Some key1 -> 
              Dump.msg (Some(fun p->p "occurs_check1 with b=%b key0=%a i2=%a key1=%a" b IU.print_in_fmtK key0 IU.print_in_fmtK i2 IU.print_in_fmtK key1)) None None;
              raise NonUnifiable
          end
      | _ -> raise NonUnifiable
    in
    let newcont t1 u1 = 
      let u1' = IU.add key0 t1 u1 in
      cont u1'
    in
    aux true newcont t2 u2

  let rec combine l = function
    | [],[]             -> l
    | (a1::l1),(a2::l2) -> combine ((a1,a2)::l) (l1,l2)
    | _,_               -> raise WrongArgumentNumber

  let unif b fold mk1 mk2 u1 u2 l =
    let rec aux l u1 u2 kkcorr = match l with
      | []         -> Some(u1,u2)
      | (t1,t2)::l -> 
        Dump.msg(Some(fun p->p "unifying %a in %a to %a in %a" IU.print_in_fmtV t1 IU.print_in_fmt u1 IU.print_in_fmtV t2 IU.print_in_fmt u2))None None; 
        let (t1',u1') = IU.expose (t1, u1) in
        let (t2',u2') = IU.expose (t2, u2) in
        try match t1', t2' with

        | (V(IU.Eigen i1),V(IU.Eigen i2)) when i1=i2 -> aux l u1' u2' kkcorr

        | (C(a,l1),C(b,l2)) -> aux (combine l (l1,l2)) u1' u2' kkcorr

        | (V(IU.Meta key1),V(IU.Meta key2)) when 
            (match KKcorr.get21 kkcorr key2 with
            | Some i -> i==key1
            | None   -> false)
            -> aux l u1' u2' kkcorr
          
        | (V(IU.Meta key1),_)  ->
          let cont u1' u2' kkcorr = match t2' with
            | V(IU.Meta key2) ->
              translate b (fold mk2) key2 u1' t1 (fun u2'' u1'' kkcorr'' -> aux l u1'' u2'' (KKcorr.swap kkcorr'')) u2' (KKcorr.swap kkcorr)
            | _ -> aux l u1' u2' kkcorr
          in
          translate b (fold mk1) key1 u2' t2 cont u1' kkcorr

        | (_,V(IU.Meta key2))  ->
          translate b (fold mk2) key2 u1' t1 (fun u2'' u1'' kkcorr'' -> aux l u1'' u2'' (KKcorr.swap kkcorr'')) u2' (KKcorr.swap kkcorr)

        | (_,_)            -> raise NonUnifiable
        with NonUnifiable -> None
    in
    aux l u1 u2 (KKcorr.init mk1 mk2)

end: sig

  val combine: ('l * 'm) list -> 'l list * 'm list -> ('l * 'm) list
  val unif: bool ->
    (MKcorr.t -> Arity.eigen -> (IU.keys -> IU.t -> IU.t) -> IU.t -> IU.t) ->
    MKcorr.t -> MKcorr.t -> IU.t -> IU.t -> (IU.values * IU.values) list 
    -> (IU.t * IU.t) option

end)

module Constraint = struct

  type t = { ar : Arity.t;
             mk : MKcorr.t;
             unifier : IU.t}

  let topconstraint = { ar  = Arity.init;
                        mk  = MKcorr.empty;
                        unifier = IU.empty}

  let print_in_fmt fmt sigma = 
    Format.fprintf fmt "{ar = {%a} || mk = {%a} || u = {%a}}" Arity.print_in_fmtEM sigma.ar MKcorr.print_in_fmt sigma.mk IU.print_in_fmt sigma.unifier

  let liftE sigma =  {
    ar      = Arity.liftE sigma.ar;
    mk      = sigma.mk;
    unifier = sigma.unifier
  }

  let projE sigma = {
    ar      = Arity.projE sigma.ar;
    mk      = sigma.mk;
    unifier = sigma.unifier
  }

  let liftM sigma =
    let (newmeta,newar) = Arity.newMeta sigma.ar in
    let (newkey,newu) = IU.add_new sigma.unifier in
    {
      ar      = newar;
      mk      = MKcorr.add sigma.mk newmeta newkey;
      unifier = newu
    }

  let projM sigma = {
    ar      = Arity.projM sigma.ar;
    mk      = ((* print_endline (Dump.toString(fun p->p "Hiding %i from %a" (sigma.ar.Arity.next_meta-1) MKcorr.print_in_fmt sigma.mk));  *)
               MKcorr.remove sigma.mk (sigma.ar.Arity.next_meta -1));
    unifier = sigma.unifier
  }

  let liftAr sigma ar =
    let diff = ar.Arity.next_meta - sigma.ar.Arity.next_meta in
    let rec liftN ui = function
      | 0 -> ui
      | n -> liftN (liftM ui) (n-1)
    in
    let tmp = liftN sigma diff in
    { ar      = ar;
      mk      = tmp.mk;
      unifier = tmp.unifier}


  let unif_aux b sigma1 sigma2 l1 l2 =
    let fold mkcorr i f =
      let bound = Arity.IntMap.find i sigma2.ar.Arity.eigen_dependencies in 
      let rec aux j u = 
        if j == bound then u
        else
          aux (j+1) (f (MKcorr.get_key mkcorr j) u)
      in aux 0
    in
    match Unification.unif b fold sigma1.mk sigma2.mk sigma1.unifier sigma2.unifier (Unification.combine [] (l1,l2)) with
    | None        -> None
    | Some(u1,u2) -> Some({ar = sigma1.ar; mk = sigma1.mk; unifier = u1},{ar = sigma2.ar; mk = sigma2.mk; unifier = u2})


  let meet_aux b sigma1 sigma2 = 
    if not(Arity.prefix sigma1.ar sigma2.ar)
    then None
    else
      let sigma1 = liftAr sigma1 sigma2.ar in
      let l1 = 
        MKcorr.fold
          (fun _ key l -> (IU.bV(IU.Meta key))::l)
          sigma1.mk
          []
      in
      let l2 = 
        MKcorr.fold
          (fun _ key l -> (IU.bV(IU.Meta key))::l)
          sigma2.mk
          []
      in
      match unif_aux b sigma1 sigma2 l1 l2 with
      | None -> None
      | Some(res,_) -> Some res

  let meet = meet_aux true

  let unif sigma l1 l2 = 
    match unif_aux true sigma sigma l1 l2 with
    | None                -> None
    | Some(sigma1,sigma2) -> meet sigma1 sigma2

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
          Dump.msg(Some(fun p -> p "Unifying atoms %a and %a" IAtom.print_in_fmt a IAtom.print_in_fmt t))None None;
          let newalias = ASet.remove a alias in
          let (b1,p1,l1) = IAtom.expose a in
          let (b2,p2,l2) = IAtom.expose t in
          (if (compare b1 b2) && (Predicates.compare p1 p2 == 0)
           then
              (Dump.msg (Some(fun p -> p "constraint = %a" Constraint.print_in_fmt sigma))None None;
              let internalise = List.map(IU.internalise (MKcorr.get_key sigma.Constraint.mk)) in
              match Constraint.unif sigma (internalise l1) (internalise l2) with
              | Some newsigma -> 
                raise (FoundIt(ASet.add a aset,newsigma, cont newalias))
              | None -> ()));
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
      (* (print_endline (Dump.toString(fun p->p "consistency on %a" ASet.print_in_fmt atomN)));  *)
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

    module PS = Tools.Prop.Structure(F)

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
