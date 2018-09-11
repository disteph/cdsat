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

(***********************)
(* Terms that are used *)
(***********************)

type datatype
  
module type Readable =
  ReadablePoly with type t         = (FreeVar.t,datatype) termF
                and type revealed := ((FreeVar.t,datatype) termF,
                                      (FreeVar.t*TermB.t free)) xterm
                and type leaf     := FreeVar.t

module type Writable =
  WritablePoly with type ('leaf,'datatype) termF := ('leaf,'datatype) termF
                        and type termB    := TermB.t
                        and type leaf     := FreeVar.t
                        and type datatype := datatype
                        and type t := (FreeVar.t,datatype) termF

module Term : Readable

module type ThTerm = sig
  type t
  (* Achtung: in (build t),
     build should NOT call reccall on term t but only on its subterms (if at all),
     otherwise this gives an infinite loop *)
  val build : reccall:(Term.t -> t) -> Term.t -> t
  val name : string
end

module Key : sig
  include Keys.S
  val make : (module ThTerm with type t = 'a) -> 'a t
end

type dsKey = DSK : _ Key.t -> dsKey

val build : dsKey list -> (module Writable)
val proj : 'a Key.t -> Term.t -> 'a

(*******************************)
(* Sets of terms that are used *)
(*******************************)

module TSet : Collection with type e = Term.t
