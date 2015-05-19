open Format

open Kernel
open Interfaces_basic
open Interfaces_theory
open General

module TermDef = struct

(* A term is either a variable or a function symbol applied to
   arguments *)

  type ('a,'b) xterm = V of 'a | C of Symbol.t * ('b list)

  module M = struct

    type ('t,'a) t = ('a,'t) xterm

    let rec equaltl eqRec = function
      | ([], []) -> true
      | ((t :: l), (t' :: l')) -> eqRec t t' && equaltl eqRec (l, l')
      | ([], (_ :: _)) | ((_ :: _), []) -> false 

    let equal eqRec eqL t1 t2 =
      match t1, t2 with
      | V(a), V(a')        -> eqL a a'
      | C(a,tl), C(a',tl') -> a = a' && equaltl eqRec (tl,tl')
      | V _, C(_, _) 
      | C(_, _), V _  -> false 

    let rec hashtl hRec = function
      | [] -> 1
      | t :: l -> hRec t + 2 * hashtl hRec l 

    let hash hRec hL t1 =
      match t1 with
      | V a    -> 1 + 2 * hL a
      | C(a,l) -> 3 * Hashtbl.hash a + 7 * hashtl hRec l
  end

  include HCons.MakePoly(M)
  type ('leaf,'datatype) term = ('leaf,'datatype) generic

  module type S = sig
    type leaf
    type datatype
    type t = (leaf,datatype) term
    val bV : leaf -> t
    val bC : Symbol.t -> t list -> t
    val clear : unit -> unit
    val print_in_fmt: Format.formatter -> t -> unit
    module Homo(Mon: MonadType) : sig
      val lift : 
        ('a -> leaf Mon.t) -> ('a,datatype) term -> (leaf,datatype) term Mon.t
      val lifttl : 
        ('a -> leaf Mon.t) -> ('a,datatype) term list -> (leaf,datatype) term list Mon.t
    end
    val subst : ('a -> leaf) -> ('a,datatype) term -> (leaf,datatype) term
  end

  module Make
    (Leaf : PHCons)
    (Data : Theory.ForParsingType with type leaf := Leaf.t) =
  struct

    include InitData
      (Basic.HashedTypeFromHCons(Leaf))
      (struct 
        type t = Data.t
        let build = function
          | V v    -> Data.leaf v
          | C(f,l) -> Data.semantic f (List.map data l)
       end)

    type datatype = Data.t

    let bV i   = build(V i)
    let bC f l = build(C(f,l))
      
    let rec print_in_fmt fmt t =
      match reveal t with
      | V a -> fprintf fmt "%a" Leaf.print_in_fmt a
      | C(f, newtl) -> fprintf fmt "%a%a" Symbol.print_in_fmt f printtl_in_fmt newtl
    and printtl_in_fmt fmt tl =
      if tl <> [] then fprintf fmt "(%a)" printrtl_in_fmt tl
    and printrtl_in_fmt fmt tl =
      match tl with
      | [] -> ()
      | t :: l ->
        if l = [] then print_in_fmt fmt t
        else fprintf fmt "%a,%a" print_in_fmt t printrtl_in_fmt l
	  
    module Homo(Mon: MonadType) = struct
      let rec lift (update: 'a -> Leaf.t Mon.t) (t: ('a,Data.t) term)
          = match reveal t with
          | V i    -> Mon.bind (fun v -> Mon.return (bV v)) (update i)
          | C(f,l) -> Mon.bind (fun l -> Mon.return (bC f l)) (lifttl update l)
      and
          lifttl (update: 'a -> Leaf.t Mon.t)
          = function
          | []    -> Mon.return []
          | t::tl -> let aux t' tl' = Mon.return(t'::tl') in
                     let aux' t' = Mon.bind (aux t') (lifttl update tl) in
                     Mon.bind aux' (lift update t)
    end

    module M = Homo(Monads.IdMon)
    let subst = M.lift
  end 
end


type ('a,'b) prod = private Prod

module AtomDef = struct 

  module M = struct

    type (_,_) t =
      AtomCons : bool * Symbol.t * (('leaf,'datatype) TermDef.term list)
      -> ('t,('leaf,'datatype)prod) t

    let equal (type a)(type b) _ _ 
        (AtomCons(b, a, tl)   :(a,b)t)
        (AtomCons(b', a', tl'):(a,b)t) = 
          b = b' && a = a' && TermDef.M.equaltl (fun t1 t2 -> TermDef.compare t1 t2 ==0) (tl, tl')

    let hash (type a)(type b) _ _ (AtomCons(b, a, tl):(a,b)t) =
      (if b then 0 else 1) + 2 * Hashtbl.hash a + 3 * TermDef.M.hashtl TermDef.id tl

  end

  module H = HCons.MakePoly(M)

  type ('leaf,'datatype) atom = (('leaf,'datatype)prod, 'datatype) H.generic

  module type S = sig
    type leaf
    type datatype
    type t = (leaf,datatype) atom
    module Term : TermDef.S with type leaf := leaf
                            and  type datatype = datatype

    val reveal  : t -> bool * Symbol.t * (Term.t list)
    val id      : t -> int 
    val compare : t -> t -> int
    val build    : bool * Symbol.t * Term.t list -> t
    val clear    : unit -> unit
    val print_in_fmt: Format.formatter -> t -> unit
    val negation : t -> t
    module Homo(Mon: MonadType) : sig
      val lift : 
        ('a -> leaf Mon.t) -> ('a,datatype) atom -> t Mon.t
    end
  end

  module Make
    (Leaf : PHCons)
    (Data : Theory.ForParsingType with type leaf := Leaf.t) =
  struct

    type leaf = Leaf.t
    type datatype = Data.t
    type t = (leaf,datatype) atom

    module Term = TermDef.Make(Leaf)(Data)
    module I = H.InitData(struct
      type t = (Leaf.t,Data.t) prod
      let equal _ _ = failwith "equal function on type (Leaf.t,Data.t) prod should not exist"
      let hash _ = failwith "hash function on type (Leaf.t,Data.t) prod should not exist"
    end)(struct
      type t = Data.t
      let build (M.AtomCons(b,p,l) (* : ((Leaf.t, Data.t) prod, Data.t) H.revealed *))
          = 
        let at = Data.semantic p (List.map TermDef.data l) in
        if b then at
        else Data.semantic Symbol.Neg [at]
    end)

    let reveal a = match H.reveal a with
      | M.AtomCons(b,p,tl) -> (b,p,tl)
    let id = H.id
    let compare = H.compare

    let build(b,p,tl) = I.build(M.AtomCons(b,p,tl))
    let clear () = Term.clear(); I.clear()

    let print_in_fmt fmt t =
      match reveal t with
      | true, s, tl ->
        if tl<>[] 
        then fprintf fmt "{%a%a}" Symbol.print_in_fmt s Term.printtl_in_fmt tl
        else fprintf fmt "{%a}" Symbol.print_in_fmt s
      | false, s, tl ->
        if tl<>[] 
        then fprintf fmt "\\overline {%a}%a" Symbol.print_in_fmt s Term.printtl_in_fmt tl
        else fprintf fmt "\\overline {%a}" Symbol.print_in_fmt s

    let negation t = let (b, a, tl) = reveal t in build(not b, a, tl)

    module Homo(Mon: MonadType) = struct

      let lift update at = 
        let module THomo = Term.Homo(Mon) in
        let (b,p,tl) = reveal at in
        Mon.bind (fun l -> Mon.return(build(b,p,l))) (THomo.lifttl update tl)

    end
  end
end

module StandardDSData
  (Leaf : PHCons)
  (Data : Theory.ForParsingType with type leaf := Leaf.t) =
struct

  module Atom = AtomDef.Make(Leaf)(Data)

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t)
      = struct

        type t =
        | TermI : Atom.Term.t -> t
        | PropI : F.t -> t

        type leaf = Leaf.t
        let leaf v = TermI(Atom.Term.bV v)

        let toForm = function
          | PropI f -> f
          | _       -> raise (Theory.ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

        let toTerm = function
          | TermI t -> t
          | _       -> raise (Theory.ModelError "ModelError: trying to convert into a term an expression that clearly is not one")


        module PropIntern = Prop.Intern(F)

        let lit (b, f, tl) = F.lit(Atom.build(b, f, tl))

        let semantic symb =
          let (o,_) = Symbol.arity symb in
          match o with
	  | Sorts.Prop -> fun l -> 
            PropI (
              try PropIntern.semantic symb (List.map toForm l)
              with Theory.ModelError _
                -> lit(true,symb,List.map toTerm l)
            )
	  | _ -> fun l -> 
	    TermI (Atom.Term.bC symb (List.map toTerm l))

        let examples = []

      end
end

module EmptyData(Leaf : PHCons) =
struct
  type t = unit option
  let semantic _ _ = None
  type leaf = Leaf.t
  let leaf     _ = None
  type form = unit
  let toForm   _ = ()
end

module StandardDS(Leaf : PHCons) = StandardDSData(Leaf)(EmptyData(Leaf))
