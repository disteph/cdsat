(* This is the implementation of formulae *)

open Format
open Interfaces_I

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
  | ForAll of 'a
  | Exists of 'a

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

module GForm : sig
  type ('a,'lit) t
  type ('a,'lit) revealt = (('a,'lit) t,'lit) form
  val reveal : ('a,'lit) t -> ('a,'lit) revealt
  val id     : ('a,'lit) t -> int
  val data   : ('a,'lit) t -> 'a
  val print_in_fmt : (formatter -> 'lit -> unit) -> formatter -> ('a,'lit) t -> unit
  val iprint_in_fmt : 
    (formatter -> 'lit -> unit)
    -> (formatter -> 'subst -> unit)
    -> formatter -> ('a,'lit) t*'subst -> unit
  val icompare : ('b->'b->int) -> (('a,'lit) t * 'b) -> (('a,'lit) t * 'b) -> int 
end

module type FormExtraInfo = sig
  type t
  type lit
  val fdata_build: (t,lit) GForm.revealt -> t
end

module type FormulaType = sig
  type datatype
  type lit
  type t   = (datatype,lit) GForm.t
  val print_in_fmt  : formatter -> t -> unit
  val iprint_in_fmt : (formatter -> 'subst -> unit) -> formatter -> (t*'subst) -> unit
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
  val forall : t -> t
  val exists : t -> t
end

module Formula(Atom: AtomType)(Fdata: FormExtraInfo with type lit = Atom.t) : 
  FormulaType with type datatype = Fdata.t and type lit = Atom.t
