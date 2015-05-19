(* This is the implementation of formulae *)

open Format
open Interfaces_theory

(* Type for a formula head:
   'a is the type of what is found below connectives
   'lit is the type of literals *)

type ('a,'lit) form =
  | Lit of 'lit
  | TrueP
  | TrueN
  | FalseP
  | FalseN
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a
  | ForAll of Sorts.t * 'a
  | Exists of Sorts.t * 'a

(* (Recursive) type for a generic formula, with identifiers:
   'a is the type of the extra information that is found at every node
   'lit is the type of literals
   reveal reveals the formula's head
   id    produces the formula's identifier
   data  produces the extra information carried at the root of the formula
   print_in_fmt can be used for pretty-printing, when given a similar function for literals
   iprint_in_fmt does the same for a formula paired with something else
   icompare compares 2 pairs (formula + something else) lexicographically
   (comparison of the formulae themselves is done by looking at their identifiers only)
*)

module Formula : sig

  include HCons.PolyS with type ('t,'lit) initial := ('t,'lit) form

  val print_in_fmt : (formatter -> 'lit -> unit) -> formatter -> ('lit,'a) generic -> unit

  val iprint_in_fmt : 
    (formatter -> 'lit -> unit)
    -> (formatter -> 'subst -> unit)
    -> formatter -> ('lit,'a) generic*'subst -> unit

  val icompare : 
    ('b->'b->int)
    -> (('lit,'a) generic * 'b)
    -> (('lit,'a) generic * 'b)
    -> int 

  module type Extra = sig
    type t
    type lit
    val build: (lit,t) revealed -> t
  end

  module type S = sig
    type datatype
    type lit
    type t   = (lit,datatype) generic
    val print_in_fmt  : formatter -> t -> unit
    val iprint_in_fmt : (formatter -> 'subst -> unit) -> formatter -> (t*'subst) -> unit
    val compare : t -> t -> int
    val negation : t -> t
    val lit    : lit -> t
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

  module Make(Atom: AtomType)(Fdata: Extra with type lit = Atom.t) : 
    S with type datatype = Fdata.t and type lit = Atom.t

end
