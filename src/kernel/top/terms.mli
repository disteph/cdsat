open Interfaces_basic
open Basic

type ('a,'b) xterm = V of 'a | C of Symbol.t * ('b list)
type ('leaf,'datatype) term

val reveal : ('leaf,'datatype) term -> ('leaf,('leaf,'datatype) term) xterm
val data   : ('leaf,'datatype) term -> 'datatype
val id     : ('leaf,'datatype) term -> int
val compare: ('leaf,'datatype) term -> ('leaf,'datatype) term -> int

val hashtl  : ('leaf,'datatype) term list -> int
val equaltl : (('leaf,'datatype) term list * ('leaf,'datatype) term list) -> bool

module type DataType = sig
  type t
  type leaf
  val bV : leaf -> t
  val bC : int -> Symbol.t -> t list -> t
end

module type S = sig
  type leaf
  type datatype
  type t = (leaf,datatype) term
  val bV : leaf -> t
  val bC : Symbol.t -> t list -> t
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
  (Data : DataType with type leaf := Leaf.t)
  : S with type leaf = Leaf.t
      and  type datatype = Data.t

module EmptyData(Leaf : PHCons)
  : DataType with type t = unit
             and  type leaf := Leaf.t

