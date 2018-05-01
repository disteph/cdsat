open Interfaces_basic
open Basic

type _ free = private Free
type bound_prim  = private Bound
type bound = Variables.BoundVar.t*bound_prim
                                    
type (_,_) xterm =
  | V  : 'l                    -> (_, 'l*_) xterm
  | C  : Symbols.t * ('a list) -> ('a,_) xterm
  | FB : Sorts.t*'a* 'l DSubst.t -> (_,'l*'a free) xterm
  | BB : Sorts.t*'a            -> ('a, bound) xterm

module type Leaf = sig
  include PH
  val get_sort : t -> Sorts.t
end

module TermB : sig

  type t [@@deriving eq, ord, show, hash]

  val bV : Variables.BoundVar.t -> t
  val bC : Symbols.t -> t list -> t
  val bB : Sorts.t -> t -> t
  val reveal : t -> (t, bound) xterm
  val get_sort : t -> Sorts.t

end

type ('leaf,'datatype) termF

val reveal : ('leaf,'datatype) termF
             -> (('leaf,'datatype) termF,('leaf*TermB.t free)) xterm
val data   : ('leaf,'datatype) termF -> 'datatype
val id     : ('leaf,'datatype) termF -> int

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
         (Data : DataType with type leaf = Leaf.t)
       : S with type datatype = Data.t
            and type leaf = Leaf.t

module EmptyData(Leaf: Leaf) : DataType with type t = unit
                                         and type leaf = Leaf.t

