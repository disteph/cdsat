open General
open Basic

module type Leaf = sig
  include PH
  val get_sort : t -> Sorts.t
end

module type Sprim = sig
  include PHCons
  val get_sort : t -> Sorts.t
end

module type Sbuild = sig
  type ('leaf,'datatype) termF
  type datatype
  type leaf
  type termB
  type t
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
  include Sbuild
  include Sprim with type t := (leaf,datatype) termF
end
