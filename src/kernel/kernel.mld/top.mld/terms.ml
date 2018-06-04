open Format
open General
open Basic
open Interfaces_basic
open Variables

type _ free = private Free
type bound_prim  = private Bound
type bound = BoundVar.t*bound_prim
                          
type (_,_) xterm =
  | V  : 'l                      -> (_, 'l*_) xterm
  | C  : Symbols.t * ('a list)   -> ('a,_) xterm
  | FB : Sorts.t*'a* 'l DSubst.t -> (_,'l*'a free) xterm
  | BB : Sorts.t*'a              -> ('a, bound) xterm

type (_,_) func_prim =
  | BoundFunc : (bound_prim,_) func_prim
  | FreeFunc : ('a -> 'b) -> ('a free,'b free) func_prim

type ('a,'b) func = ('a,'b free) func_prim

let equal (type l b a)
      (eqLeaf: l -> l -> bool)
      (eqSub:(b,(b,bool)func)func)
      eqRec
      (t1:(a,l*b)xterm)
      (t2:(a,l*b)xterm) =
  match t1,t2 with
  | V l1, V l2          -> eqLeaf l1 l2
  | C(s1,l1), C(s2,l2)  -> Symbols.equal s1 s2 && List.equal eqRec l1 l2
  | FB(s1,x1,d1), FB(s2,x2,d2)-> let FreeFunc eqSub = eqSub in
                                 let FreeFunc eqSub = eqSub x1 in
                                 eqSub x2 && DSubst.equal eqLeaf d1 d2
  | BB(s1,x1), BB(s2,x2) -> eqRec x1 x2
  | _, _                       -> false


let hash (type a l b)
      (hLeaf:l->int) (hSub:(b,int)func) hRec : (a,l*b)xterm -> int  = function
  | V l        -> hLeaf l
  | C(symb,l)  -> 5*(Symbols.hash symb)+17*(List.hash hRec l)
  | FB(so,x,d) -> let FreeFunc hSub = hSub in
                  19*Sorts.hash so+31*(hSub x)+23*(Hash.wrap1 DSubst.hash_fold_t hLeaf d)
  | BB(so,x)   -> 31*(hRec x)

(* Displays a formula *)
let pp (type a l b)
      (pLeaf:formatter->l->unit)
      (pSub:(b,formatter->unit)func)
      pRec =
  let aux fmt: (a,l*b)xterm -> unit = function
    | V l        -> fprintf fmt "%a" pLeaf l
    | C(symb,[]) -> fprintf fmt "%a" Symbols.pp symb
    | C(symb,l)  -> fprintf fmt "%a%a" Symbols.pp symb (List.pp ~sep:", " ~wrap:("(",")") pRec) l
    | FB(so,f,d) -> let FreeFunc pif = pSub in
                    fprintf fmt "%a%a" (fun fmt a -> pif a fmt) f (DSubst.pp pLeaf) d
    | BB(so,f)   -> fprintf fmt "%a" pRec f
  in fun reveal fmt t -> aux fmt (reveal t)

                             
let get_sort (type a l b)
      sLeaf
      (sSub:(b,Sorts.t)func)
      sRec =
  let aux : (a,l*b)xterm -> Sorts.t = function
    | V bv      -> sLeaf bv
    | C(symb,_) -> let so,_ = Symbols.arity symb in so
    | FB(_,t,_) -> let FreeFunc sSub = sSub in
                   sSub t
    | BB(_,f)   -> sRec f
  in fun reveal t -> aux (reveal t)

module type Leaf = sig
  include PH
  val get_sort : t -> Sorts.t
end

                     
(******************************)
(* Terms with bound variables *)
(******************************)

module TermB = struct

  module B = struct
    type nonrec 'a t = ('a,bound) xterm
    let equal eqRec = equal BoundVar.equal BoundFunc eqRec 
    let hash hRec = hash BoundVar.hash BoundFunc hRec
    let hash_fold_t hash_fold_a = Hash.hash2fold(hash(Hash.fold2hash hash_fold_a))
    let name = "TermB"
  end

  include HCons.Make(B)
  include Init(B)

  let pp =
    let rec aux fmt t = pp BoundVar.pp BoundFunc aux reveal fmt t
    in aux
  let show = Print.stringOf pp

  let get_sort =
    let rec aux t = get_sort BoundVar.get_sort BoundFunc aux reveal t
    in aux

  let bV i   = build(V i)
  let bC f l = build(C(f,l))
  let bB so t = build(BB(so,t))

end



(******************************)
(* Terms with bound variables *)
(******************************)

                 
module F = struct
  type ('a,'l) t = ('a, 'l * TermB.t free) xterm
  let equal eqRec eqPar = equal
                            eqPar
                            (FreeFunc(fun x->FreeFunc(TermB.equal x)))
                            eqRec
  let hash hRec hPar = hash hPar (FreeFunc TermB.hash) hRec
  let hash_fold_t hash_fold_a hash_fold_l
    = Hash.hash2fold(hash(Hash.fold2hash hash_fold_a)(Hash.fold2hash hash_fold_l))
  let name = "TermF"
end

include HCons.MakePoly(F)

type ('leaf,'datatype) termF = ('leaf*'datatype*[`HCons]) G.t
                                                 
module type DataType = sig
  type t
  type leaf
  val bV : int -> leaf -> t
  val bC : int -> Symbols.t -> t list -> t
  val bB : int -> Sorts.t*TermB.t*leaf DSubst.t -> t
end

module type S = sig
  type datatype
  type leaf
  include PHCons with type t = (leaf,datatype) termF
  val print_of_id: Format.formatter -> int -> unit
  val get_sort : t -> Sorts.t
  val term_of_id: int -> t
  val bV : leaf -> t
  val bC : Symbols.t -> t list -> t
  val bB : Sorts.t -> TermB.t -> leaf DSubst.t -> t
  module Homo(Mon: MonadType) : sig
    val lift :
      ('a -> leaf Mon.t) -> ('a,_) termF -> (leaf,datatype) termF Mon.t
    val lifttl :
      ('a -> leaf Mon.t) -> ('a,_) termF list -> (leaf,datatype) termF list Mon.t
  end
  val subst : ('a -> leaf) -> ('a,_) termF -> (leaf,datatype) termF
  val lift : leaf DSubst.t -> TermB.t -> t
end

module Make(Leaf: Leaf)
         (Data : DataType with type leaf = Leaf.t) =
  struct

    type leaf = Leaf.t
                  
    include InitData(HCons.BackIndex)(Leaf)
              (struct 
                type t = Data.t
                let build tag = function
                  | V v     -> Data.bV tag v
                  | C(f,l)  -> Data.bC tag f (List.map data l)
                  | FB(so,t,d) -> Data.bB tag (so,t,d)
              end)

    type datatype = Data.t

    let Opt.Some term_of_id = backindex

    let id = id
    let compare = compare
    let bV i   = build(V i)
    let bC f l = build(C(f,l))
    let bB so t d = build(FB(so,t,d))    

    let pp fmt t =
      let rec aux fmt t =
        pp Leaf.pp (FreeFunc(fun t fmt->TermB.pp fmt t))
          aux reveal fmt t
      in aux fmt t

    let show a = Print.stringOf pp a
                        
    let print_of_id fmt index =
      let atom = term_of_id index in
      pp fmt atom
         
    let get_sort =
      let rec aux t =
        get_sort Leaf.get_sort (FreeFunc(TermB.get_sort)) aux reveal t
      in aux

    module Homo(Mon: MonadType) = struct

      let rec lift update t
        =
        match reveal t with
        | V i    -> Mon.bind (fun v -> Mon.return (bV v)) (update i)
        | C(f,l) -> Mon.bind (fun l -> Mon.return (bC f l)) (lifttl update l)
        | FB(so,t,d) -> Mon.bind(fun d -> Mon.return(bB so t d)) (liftd update d)
      and lifttl update
        = function
        | []    -> Mon.return []
        | t::tl -> let aux t' tl' = Mon.return(t'::tl') in
                   let aux' t' = Mon.bind (aux t') (lifttl update tl) in
                   Mon.bind aux' (lift update t)
      and liftd update
        = function
        | []        -> Mon.return []
        | (t,w)::tl -> let aux t' tl' = Mon.return((t',w)::tl') in
                       let aux' t' = Mon.bind (aux t') (liftd update tl) in
                       Mon.bind aux' (update t)
    end

    module M = Homo(Basic.IdMon)
    let subst = M.lift

    let get_in_subst d intso = 
      let fv,_  = BoundVar.get_from_context intso
                    (fun k -> DSubst.get (fun fmt v->pp fmt (bV v)) k d) in
      fv

    let rec lift d t =
      match TermB.reveal t with
      | V i      -> bV (get_in_subst d i)
      | C(f,l)   -> bC f (List.map (lift d) l)
      | BB(so,t) -> bB so t d

  end 

module EmptyData(Leaf:Leaf) = struct
  type t = unit
  type leaf = Leaf.t
  let bC _ _ _ = ()
  let bV _ _   = ()
  let bB _ _   = ()
end
