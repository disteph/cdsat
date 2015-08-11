(*****************************************************)
(* This file contains the implementation of formulae *)
(*****************************************************)

open Format

open Top
open Symbols
open Interfaces_basic
open Basic
open Variables

open Literals
open Interfaces_theory

type 'a free = private Free
type bound = private Bound

type (_,_) form =
| Lit  : LitF.t -> (_,_ free) form
| LitB : LitB.t -> (_,bound) form
| TrueP: (_,_) form
| TrueN: (_,_) form
| FalseP: (_,_) form
| FalseN: (_,_) form
| AndP  : 'a * 'a -> ('a,_) form
| OrP   : 'a * 'a -> ('a,_) form
| AndN  : 'a * 'a -> ('a,_) form
| OrN   : 'a * 'a -> ('a,_) form
| ForAll: Sorts.t * 'a * DSubst.t -> (_,'a free) form
| Exists: Sorts.t * 'a * DSubst.t -> (_,'a free) form
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

  val print_in_fmt : formatter -> _ generic -> unit

  module type Extra = sig
    type t
    val build: t g_revealed -> t
  end

  module type S = sig
    type datatype

    include PHCons with type t = datatype generic

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
