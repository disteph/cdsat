open Interfaces_basic
open Basic

type ('a,'b) xterm = V of 'a | C of Symbols.t * ('b list)
type ('leaf,'datatype) term

val reveal : ('leaf,'datatype) term -> ('leaf,('leaf,'datatype) term) xterm
val data   : ('leaf,'datatype) term -> 'datatype

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
  (Data : DataType with type leaf := Leaf.t)
  : S with type leaf = Leaf.t
      and  type datatype = Data.t

module EmptyData(Leaf : PHCons)
  : DataType with type t = unit
             and  type leaf := Leaf.t

