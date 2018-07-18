open Basic

module type Leaf = sig
  include PH
  val get_sort : t -> Sorts.t
end

module type DataType = sig
  type ('leaf,'datatype) termF
  type t
  type leaf
  val build : (leaf,'d) termF -> t
end

module type S = sig
  type ('leaf,'datatype) termF
  type termB
  type datatype
  type leaf
  include PHCons with type t = (leaf,datatype) termF
  val get_sort : t -> Sorts.t
  val bV : leaf -> t
  val bC : Symbols.t -> t list -> t
  val bB : Sorts.t -> termB -> leaf DSubst.t -> t
  module Homo(Mon: MonadType) : sig
    val lift :
      ('a -> leaf Mon.t) -> ('a,_) termF -> (leaf,datatype) termF Mon.t
    val lifttl :
      ('a -> leaf Mon.t) -> ('a,_) termF list -> (leaf,datatype) termF list Mon.t
  end
  val subst : ('a -> leaf) -> ('a,_) termF -> (leaf,datatype) termF
  val lift : leaf DSubst.t -> termB -> t
end
