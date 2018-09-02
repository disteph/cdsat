open Format
open General
open Basic
open Variables

include Terms_sig
    
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
      (eqLeaf: l Equal.t)
      (eqSub :(b,(b,bool)func)func)
      (eqRec : a Equal.t)
      (t1    :(a,l*b)xterm)
      (t2    :(a,l*b)xterm) =
  match t1,t2 with
  | V l1, V l2          -> eqLeaf l1 l2
  | C(s1,l1), C(s2,l2)  -> Symbols.equal s1 s2 && List.equal eqRec l1 l2
  | FB(s1,x1,d1), FB(s2,x2,d2)-> let FreeFunc eqSub = eqSub in
                                 let FreeFunc eqSub = eqSub x1 in
                                 eqSub x2 && DSubst.equal eqLeaf d1 d2
  | BB(s1,x1), BB(s2,x2) -> eqRec x1 x2
  | _, _                       -> false

let hash_fold (type a l b)
    (hash_fold_l:l Hash.folder)
    (hSub: (b,Hash.state -> Hash.state)func)
    hash_fold_a : (a,l*b) xterm Hash.folder  = fun state xterm -> match xterm with
  | V l        -> [%hash_fold:int*l] state (3,l)
  | C(symb,l)  -> [%hash_fold:int*Symbols.t*(a list)] state (5,symb,l)
  | FB(so,x,d) -> let FreeFunc hash_fold_b = hSub in
    [%hash_fold:int*Sorts.t*(l DSubst.t)] state (7,so,d)
    |> hash_fold_b x
  | BB(so,x)   -> [%hash_fold:int*Sorts.t*a] state (11,so,x)


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
  in fun reveal t -> t |> reveal |> aux

                     
(******************************)
(* Terms with bound variables *)
(******************************)

module TermB = struct

  module B = struct
    type 'a t = ('a,bound) xterm
    let equal eqRec = equal BoundVar.equal BoundFunc eqRec 
    let hash_fold_t hRec = hash_fold BoundVar.hash_fold_t BoundFunc hRec
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
(* Terms with other variables *)
(******************************)
                 
module F = struct
  type ('a,'l) t = ('a, 'l * TermB.t free) xterm
end

include HCons.MakePoly(F)

type ('leaf,'datatype) termF = ('leaf*'datatype*[`HCons]) G.t
                                                 

module Make(Leaf: Leaf)(Data : sig type t end) =
  struct

    module F = struct
      type 'a t = ('a, Leaf.t) F.t
      let equal eqRec = equal
          Leaf.equal
          (FreeFunc(fun x->FreeFunc(TermB.equal x)))
          eqRec
      let hash_fold_t hashRec
        = hash_fold
          Leaf.hash_fold_t
          (FreeFunc(fun x state -> TermB.hash_fold_t state x))
          hashRec
      let name = "TermF"
    end

    include InitData(Leaf)(F)(Data)

    let pp fmt t =
      let rec aux fmt t =
        pp Leaf.pp (FreeFunc(fun t fmt->TermB.pp fmt t))
          aux reveal fmt t
      in aux fmt t

    let show a = Print.stringOf pp a

    let get_sort =
      let rec aux t =
        get_sort Leaf.get_sort (FreeFunc(TermB.get_sort)) aux reveal t
      in aux

    module Build(D: sig val build : t -> Data.t end) = struct

      type leaf = Leaf.t
      type datatype = Data.t
      type termB = TermB.t           
        
      let build = build D.build

      let bV i   = build(V i)
      let bC f l = build(C(f,l))
      let bB so t d = build(FB(so,t,d))    

      module Homo(Mon: Monads.Monad) = struct

        include Monads.Make_Let(Mon)

        let rec lift update t = match reveal t with
          | V i    -> let%map v = update i in bV v
          | C(f,l) -> let%map l = lifttl update l in bC f l
          | FB(so,t,d) ->
            let aux (i,w) = Mon.bind(fun sofar -> let%map i' = update i in (i',w)::sofar) in
            let%map d = List.fold aux d (Mon.return []) in
            bB so t d
        and lifttl update l =
          let aux t = Mon.bind(fun sofar -> let%map t' = lift update t in t'::sofar) in
          List.fold aux l (Mon.return [])

      end

      module M = Homo(Monads.IdMon)
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
  end 


(***********************)
(* Terms that are used *)
(***********************)

module ThTermKey = Keys.Make()

module ThTerm = Hashtbl_hetero.MakeT(ThTermKey)

module Term = Make(FreeVar)(ThTerm)

module TSet = MakePATCollection(Term)
