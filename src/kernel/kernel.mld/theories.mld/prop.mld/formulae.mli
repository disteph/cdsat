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
open Tools
       
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
| ForAllF: Sorts.t * 'a * FVSubst.t -> (_,'a free) form
| ExistsF: Sorts.t * 'a * FVSubst.t -> (_,'a free) form
| ForAllB: Sorts.t * 'a -> ('a,bound) form
| ExistsB: Sorts.t * 'a -> ('a,bound) form


module FormulaF : sig

  include HCons.S with type 'a initial = ('a,Terms.TermB.t free) form

  val print_in_fmt : ?print_atom:(formatter -> int -> unit) -> formatter -> 'a generic -> unit

  module type Extra = sig
    type t
    val build: t g_revealed -> t
  end

  module type S = sig
    type datatype

    type t = datatype generic [@@deriving eq,hash,ord]
    val id : t -> int
    val clear : unit -> unit
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
    val forall : Sorts.t * Terms.TermB.t * Tools.FVSubst.t -> t
    val exists : Sorts.t * Terms.TermB.t * Tools.FVSubst.t -> t
      
    val bV : int -> FreeVar.t -> t
    val bC : int -> Symbols.t  -> t list -> t
    val bB : int -> (Sorts.t*Terms.TermB.t*Tools.FVSubst.t) -> t
  end

  module Make(Fdata: Extra) : S with type datatype = Fdata.t

end
