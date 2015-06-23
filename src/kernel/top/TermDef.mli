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
      ('a -> leaf Mon.t) -> ('a,datatype) term -> (leaf,datatype) term Mon.t
    val lifttl :
      ('a -> leaf Mon.t) -> ('a,datatype) term list -> (leaf,datatype) term list Mon.t
  end
  val subst : ('a -> leaf) -> ('a,datatype) term -> (leaf,datatype) term
end

module Make
  (Leaf : PHCons)
  (Data : Semantic with type leaf := Leaf.t)
  : S with type leaf = Leaf.t
      and  type datatype = Data.t

