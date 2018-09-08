open General
open Basic

module type Leaf = sig
  include PH
  val get_sort : t -> Sorts.t
end

module type ReadablePoly = sig
  include PHCons
  type leaf
  type revealed
  val reveal : t -> revealed
  val get_sort : t -> Sorts.t
end

module type WritablePoly = sig
  type ('leaf,'datatype) termF
  type leaf
  type datatype
  type t = (leaf,datatype) termF
  type termB
  val bV : leaf -> t
  val bC : Symbols.t -> t list -> t
  val bB : Sorts.t -> termB -> leaf DSubst.t -> t
  module Homo(Mon: Monads.Monad) : sig
    val lift :
      ('a -> leaf Mon.t) -> ('a,_) termF -> (leaf,datatype) termF Mon.t
    val lifttl :
      ('a -> leaf Mon.t) -> ('a,_) termF list -> (leaf,datatype) termF list Mon.t
  end
  val subst : ('a -> leaf) -> ('a,_) termF -> (leaf,datatype) termF
  val lift : leaf DSubst.t -> termB -> t
end

module type S = sig
  include WritablePoly
  include ReadablePoly with type t := (leaf,datatype) termF
                        and type leaf := leaf
end
