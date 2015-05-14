open Format

open Kernel
open Interfaces_basic
open Interfaces_theory
open General

module TermDef = struct

(* A term is either a variable or a function symbol applied to
   arguments *)

  type ('a,'b) xterm = V of 'a | C of Symbol.t * ('b list)

  type ('leaf,'datatype) term = {
    reveal : ('leaf,('leaf,'datatype) term) xterm;
    id : int;
    data: 'datatype
  }
    
  let reveal f = f.reveal
  let data   f = f.data

  module type Type = sig
    type leaf
    type datatype
    type t = (leaf,datatype) term
    val bV : leaf -> t
    val bC : Symbol.t -> t list -> t
    val id     : t -> int
    val print_in_fmt: Format.formatter -> t -> unit
    val clear : unit -> unit
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

    type leaf = Leaf.t
    type datatype = Data.t

    type t    = (leaf,datatype) term

    let id f  = f.id

    let rec equaltl = function
      | ([], []) -> true
      | ((t :: l), (t' :: l')) -> t == t' && equaltl (l, l')
      | ([], (_ :: _)) | ((_ :: _), []) -> false 

    let equal t1 t2 =
      match t1.reveal, t2.reveal with
      | V(a), V(a')        -> Leaf.compare a a' == 0
      | C(a,tl), C(a',tl') -> a = a' && equaltl(tl,tl')
      | V _, C(_, _) 
      | C(_, _), V _  -> false 

    let rec hashtl = function
      | [] -> 1
      | t :: l -> t.id + 2 * hashtl l 

    let hash t1 =
      match t1.reveal with
      | V a    -> 1 + 2 * Leaf.id a
      | C(a,l) -> 3 * Hashtbl.hash a + 7 * hashtl l

    module H = Hashtbl.Make(struct
      type t1 = t
      type t = t1
      let hash = hash
      let equal = equal
    end)

    let table = H.create 5003

    let atomid = ref 0

    let build a =
      let aux = function
        | V v    -> Data.leaf v
        | C(f,l) -> Data.semantic f (List.map data l)
      in
      let f = {reveal = a; id = !atomid; data = aux a} in
      try H.find table f
      with Not_found ->
        incr atomid;
        H.add table f f;
        f

    let bV i   = build(V i)
    let bC f l = build(C(f,l))
      
    let rec print_in_fmt fmt t =
      match t.reveal with
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
	  
    let clear () = atomid := 0; H.clear table

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


module Predicates = struct
  type t = {reveal : Symbol.t; id : int}
  let reveal t = t.reveal
  let id t = t.id
  let table  = Hashtbl.create 5003 
  let predid = ref 0
  let build a =
    let f = {reveal = a; id = !predid} in
    try Hashtbl.find table a
    with Not_found ->
      incr predid;
      Hashtbl.add table a f;
      f
  let compare s s' = Pervasives.compare s.id s'.id
  let clear () = predid := 0;  Hashtbl.clear table
end


module AtomDef = struct 

  type ('leaf,'datatype) atom = {
    reveal : bool * Predicates.t * (('leaf,'datatype) TermDef.term list);
    id     : int;
    data   : 'datatype
  }

  let reveal t = t.reveal
  let data   t = t.data

  module type Type = sig
    type leaf
    type datatype
    type t = (leaf,datatype) atom
    module Term : TermDef.Type with type leaf = leaf
                               and  type datatype = datatype
    val build  : bool * Predicates.t * Term.t list -> t
    val bbuild : bool * Symbol.t * Term.t list -> t
    val id : t -> int 
    val negation : t -> t
    val print_in_fmt: Format.formatter -> t -> unit
    val compare : t -> t -> int
    val clear   : unit -> unit
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
        
    module Term = TermDef.Make(Leaf)(Data)
      
    type t = (Leaf.t,Data.t) atom

    let id t     = t.id

    let equal t t'= 
      let (b, a, tl) = t.reveal in
      let (b', a', tl') = t'.reveal in
      b = b' && Predicates.compare a a' == 0 && Term.equaltl (tl, tl')

    let hash t =
      let (b, a, tl) = t.reveal in
      (if b then 0 else 1) + 2 * Hashtbl.hash a + 3 * Term.hashtl tl

    module H = Hashtbl.Make(struct
      type t1 = t
      type t = t1
      let equal = equal
      let hash = hash
    end)

    let print_in_fmt fmt t =
      match t.reveal with
      | (true, s, tl) ->
        if tl<>[] then fprintf fmt "{%a%a}" Symbol.print_in_fmt (Predicates.reveal s) Term.printtl_in_fmt tl
        else fprintf fmt "{%a}" Symbol.print_in_fmt (Predicates.reveal s)
      | (false, s, tl) ->
        if tl<>[] then fprintf fmt "\\overline {%a}%a" Symbol.print_in_fmt (Predicates.reveal s) Term.printtl_in_fmt tl
        else fprintf fmt "\\overline {%a}" Symbol.print_in_fmt (Predicates.reveal s)

    let table = H.create 5003

    let attomid = ref 0

    let build a =
      let aux (b,p,l) =
        let at = Data.semantic (Predicates.reveal p) (List.map TermDef.data l) in
        if b then at 
        else Data.semantic Symbol.Neg [at]
      in
      let f = {reveal = a; id = !attomid; data = aux a} in
      try H.find table f
      with Not_found ->
        incr attomid;
        H.add table f f;
        f

    let bbuild (b,s,tl) = build (b, Predicates.build s, tl)

    let negation t = let (b, a, tl) = t.reveal in build (not b, a, tl)

    let compare t t' = Pervasives.compare t.id t'.id

    let clear () =
      attomid := 0;
      Predicates.clear();
      Term.clear();
      H.clear table

    module Homo(Mon: MonadType) = struct

      let lift update at = 
        let module M = Term.Homo(Mon) in
        let (b,p,tl) = reveal at in
        Mon.bind (fun l -> Mon.return(build(b,p,l))) (M.lifttl update tl)

    end
  end
end

module StandardDSData
  (Leaf : PHCons)
  (Data : Theory.ForParsingType with type leaf := Leaf.t) =
struct

  module Atom = AtomDef.Make(Leaf)(Data)

  module ForParsingWOEx(F: Kernel.Formulae.FormulaType with type lit = Atom.t)
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

        let lit (b, f, tl) = F.lit(Atom.bbuild(b, f, tl))

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
