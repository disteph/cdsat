open Format
open General
open Interfaces_basic

(* A term is either a variable or a function symbol applied to
   arguments *)

type ('a,'b) xterm = V of 'a | C of Symbols.t * ('b list) [@@deriving eq, hash]

module M = struct
  type ('t,'a) t = ('a,'t) xterm [@@deriving eq, hash]
  let hash hRec hL = Hash.wrap2 hash_fold_t hRec hL
end

include HCons.MakePoly(M)

type ('leaf,'datatype) term = ('leaf,'datatype) generic
                          
module type DataType = sig
  type t
  type leaf
  val bV : int -> leaf -> t
  val bC : int -> Symbols.t -> t list -> t
end

module type S = sig
  type leaf
  type datatype
  include PHCons with type t = (leaf,datatype) term
  val pp_tl: Format.formatter -> t list -> unit
  val print_of_id: Format.formatter -> int -> unit
  val term_of_id: int -> t
  val bV : leaf -> t
  val bC : Symbols.t -> t list -> t
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
    (Leaf)
    (struct 
      type t = Data.t
      let build tag = function
        | V v    -> Data.bV tag v
        | C(f,l) -> Data.bC tag f (List.map data l)
     end)

  type datatype = Data.t
  type leaf = Leaf.t

  let Opt.Some term_of_id = backindex

  let id = id
  let compare = compare
  let bV i   = build(V i)
  let bC f l = build(C(f,l))
    
  let rec pp fmt t =
    match reveal t with
    | V a -> fprintf fmt "%a" Leaf.pp a
    | C(f, newtl) -> fprintf fmt "%a%a" Symbols.pp f pp_tl newtl
  and pp_tl fmt tl = match tl with
    | []   -> ()
    | _::_ -> fprintf fmt "(%a)" pp_rtl tl
  and pp_rtl fmt tl =
    match tl with
    | [] -> ()
    | [t] -> pp fmt t
    | t :: l -> fprintf fmt "%a,%a" pp t pp_rtl l

  let show = Dump.stringOf pp

  let print_of_id fmt index =
    let atom = term_of_id index in
    pp fmt atom
                 
  module Homo(Mon: MonadType) = struct
    let rec lift update t
      = match reveal t with
      | V i    -> Mon.bind (fun v -> Mon.return (bV v)) (update i)
      | C(f,l) -> Mon.bind (fun l -> Mon.return (bC f l)) (lifttl update l)
    and lifttl update
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
