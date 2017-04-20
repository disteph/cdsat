(*****************************************************)
(* This file contains the implementation of formulae *)
(*****************************************************)

open Format
open General
open Top
open Symbols
open Interfaces_basic
open Basic
open Variables

open Termstructures.Literals

type 'a free = private Free
type bound = private Bound

type (_,_) form =
| LitF : LitF.t -> (_,_ free) form
| LitB : LitB.t -> (_,bound) form
| TrueP: (_,_) form
| TrueN: (_,_) form
| FalseP: (_,_) form
| FalseN: (_,_) form
| AndP  : 'a * 'a -> ('a,_) form
| OrP   : 'a * 'a -> ('a,_) form
| AndN  : 'a * 'a -> ('a,_) form
| OrN   : 'a * 'a -> ('a,_) form
| ForAllF: Sorts.t * 'a * DSubst.t -> (_,'a free) form
| ExistsF: Sorts.t * 'a * DSubst.t -> (_,'a free) form
| ForAllB: Sorts.t * 'a -> ('a,bound) form
| ExistsB: Sorts.t * 'a -> ('a,bound) form


module FormulaB : sig
  include PHCons
  val reveal: t -> (t,bound) form
  val negation : t -> t
  val lit    : bool * TermB.t -> t
  val trueN  : t
  val trueP  : t
  val falseN : t
  val falseP : t
  val andN   : t * t -> t
  val andP   : t * t -> t
  val orN    : t * t -> t
  val orP    : t * t -> t
  val forall : Sorts.t * t -> t
  val exists : Sorts.t * t -> t
end

module FormulaF : sig

  include HCons.S with type 'a initial = ('a,FormulaB.t free) form

  val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> formatter -> 'a generic -> unit

  module type Extra = sig
    type t
    val build: t g_revealed -> t
  end

  module type S = sig
    type datatype

    type t = datatype generic [@@deriving eq,hash]
    val id : t -> int
    val clear : unit -> unit
    val compare : t -> t -> int
    val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> formatter -> t -> unit

    type revealed  = datatype g_revealed

    val negation : t -> t

    val lit    : LitF.t -> t
    val trueN  : t
    val trueP  : t
    val falseN : t
    val falseP : t
    val andN   : t * t -> t
    val andP   : t * t -> t
    val orN    : t * t -> t
    val orP    : t * t -> t
    val forall : Sorts.t * FormulaB.t * DSubst.t -> t
    val exists : Sorts.t * FormulaB.t * DSubst.t -> t

    val bV : int -> FreeVar.t -> t
    val bC : int -> Symbols.t  -> t list -> t
  end

  module Make(Fdata: Extra) : S with type datatype = Fdata.t

end
