open General
open Basic
open Variables

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

(* val data   : (_,'datatype) termF -> 'datatype
 * 
 * module Make(Leaf: Leaf)(Data : sig type t end)
 *   : sig
 *     include Sprim with type t = (Leaf.t,Data.t) termF
 *     module Build(D: sig val build : t -> Data.t end) :
 *       Sbuild  with type ('leaf,'datatype) termF := ('leaf,'datatype) termF
 *                and type datatype = Data.t
 *                and type leaf     = Leaf.t
 *                and type termB    = TermB.t
 *                and type t := t
 *   end *)

module ThTermKey : Keys.S

module ThTerm : Hashtbl_hetero.T with type 'a key = 'a ThTermKey.t

module Term : sig
    include Sprim with type t = (FreeVar.t,ThTerm.t) termF
    module Build(D: sig val build : t -> ThTerm.t end) :
      Sbuild  with type ('leaf,'datatype) termF := ('leaf,'datatype) termF
               and type datatype = ThTerm.t
               and type leaf     = FreeVar.t
               and type termB    = TermB.t
               and type t := t
  end

val proj : 'a ThTermKey.t -> Term.t -> 'a

module TSet : Collection with type e = Term.t
