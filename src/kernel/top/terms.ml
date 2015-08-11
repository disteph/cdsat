open Format

open Interfaces_basic

(* A term is either a variable or a function symbol applied to
   arguments *)

type ('a,'b) xterm = V of 'a | C of Symbols.t * ('b list)

module M = struct

  type ('t,'a) t = ('a,'t) xterm

  let rec equaltl eqRec tl1 tl2 =
    match tl1,tl2 with
    | ([], []) -> true
    | ((t :: l), (t' :: l')) -> eqRec t t' && equaltl eqRec l l'
    | ([], (_ :: _)) | ((_ :: _), []) -> false 

  let equal eqRec eqL t1 t2 =
    match t1, t2 with
    | V(a), V(a')        -> eqL a a'
    | C(a,tl), C(a',tl') -> a = a' && equaltl eqRec tl tl'
    | V _, C(_, _) 
    | C(_, _), V _  -> false 

  let rec hashtl hRec = function
    | [] -> 1
    | t :: l -> hRec t + 2 * hashtl hRec l 

  let hash hRec hL = function
    | V a    -> 1 + 2 * hL a
    | C(a,l) -> 3 * Hashtbl.hash a + 7 * hashtl hRec l
end


include HCons.MakePoly(M)

type ('leaf,'datatype) term = ('leaf,'datatype) generic

let equal t1 t2 = compare t1 t2 ==0
let equaltl tl1 tl2 = M.equaltl equal tl1 tl2
let hash  = id
let hashtl tl = M.hashtl hash tl

module type DataType = sig
  type t
  type leaf
  val bV : int -> leaf -> t
  val bC : int -> Symbols.t -> t list -> t
end

module type S = sig
  type leaf
  type datatype
  type t = (leaf,datatype) term
  val term_of_id: int -> t
  val bV : leaf -> t
  val bC : Symbols.t -> t list -> t
  val clear : unit -> unit
  val print_in_fmt: Format.formatter -> t -> unit
  val printtl_in_fmt: Format.formatter -> t list -> unit
  module Homo(Mon: MonadType) : sig
    val lift : 
      ('a -> leaf Mon.t) -> ('a,_) term -> (leaf,datatype) term Mon.t
    val lifttl : 
      ('a -> leaf Mon.t) -> ('a,_) term list -> (leaf,datatype) term list Mon.t
  end
  val subst : ('a -> leaf) -> ('a,_) term -> (leaf,datatype) term
end

module Make
  (Leaf : PHCons)
  (Data : DataType with type leaf := Leaf.t) =
struct

  include InitData(HCons.BackIndex)
    (Basic.HashedTypeFromHCons(Leaf))
    (struct 
      type t = Data.t
      let build tag = function
        | V v    -> Data.bV tag v
        | C(f,l) -> Data.bC tag f (List.map data l)
     end)

  type datatype = Data.t
  type leaf = Leaf.t

  let HCons.SomeGADT term_of_id = backindex

  let bV i   = build(V i)
  let bC f l = build(C(f,l))
    
  let rec print_in_fmt fmt t =
    match reveal t with
    | V a -> fprintf fmt "%a" Leaf.print_in_fmt a
    | C(f, newtl) -> fprintf fmt "%a%a" Symbols.print_in_fmt f printtl_in_fmt newtl
  and printtl_in_fmt fmt tl =
    if tl <> [] then fprintf fmt "(%a)" printrtl_in_fmt tl
  and printrtl_in_fmt fmt tl =
    match tl with
    | [] -> ()
    | t :: l ->
      if l = [] then print_in_fmt fmt t
      else fprintf fmt "%a,%a" print_in_fmt t printrtl_in_fmt l
	
  module Homo(Mon: MonadType) = struct
    let rec lift update t
        = match reveal t with
        | V i    -> Mon.bind (fun v -> Mon.return (bV v)) (update i)
        | C(f,l) -> Mon.bind (fun l -> Mon.return (bC f l)) (lifttl update l)
    and
        lifttl update
        = function
        | []    -> Mon.return []
        | t::tl -> let aux t' tl' = Mon.return(t'::tl') in
                   let aux' t' = Mon.bind (aux t') (lifttl update tl) in
                   Mon.bind aux' (lift update t)
  end

  module M = Homo(Basic.IdMon)
  let subst = M.lift
end 

module EmptyData(Leaf : PHCons) =
struct
  type t = unit
  let bC _ _ _ = ()
  let bV _ _   = ()
end
