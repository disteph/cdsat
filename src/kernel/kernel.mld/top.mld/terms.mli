open Basic

include module type of Terms_sig
  
type _ free = private Free
type bound_prim  = private Bound
type bound = Variables.BoundVar.t*bound_prim
                                    
type (_,_) xterm =
  | V  : 'l                      -> (_, 'l*_) xterm
  | C  : Symbols.t * ('a list)   -> ('a,_) xterm
  | FB : Sorts.t*'a* 'l DSubst.t -> (_,'l*'a free) xterm
  | BB : Sorts.t*'a              -> ('a, bound) xterm

module TermB : sig

  type t [@@deriving eq, ord, show, hash]

  val bV : Variables.BoundVar.t -> t
  val bC : Symbols.t -> t list -> t
  val bB : Sorts.t -> t -> t
  val reveal : t -> (t, bound) xterm
  val get_sort : t -> Sorts.t

end

type ('leaf,'datatype) termF

val reveal : ('l,'d) termF -> (('l,'d) termF,('l*TermB.t free)) xterm
val data   : (_,'datatype) termF -> 'datatype

module Make(Leaf: Leaf)
    (Data : DataType with type ('leaf,'datatype) termF := ('leaf,'datatype) termF
                      and type leaf := Leaf.t)
  : S with type ('leaf,'datatype) termF := ('leaf,'datatype) termF
       and type termB := TermB.t
       and type datatype = Data.t
       and type leaf   = Leaf.t

module EmptyData(Leaf: Leaf) : DataType with type ('l,'d) termF := ('l,'d) termF
                                         and type t = unit
                                         and type leaf = Leaf.t

