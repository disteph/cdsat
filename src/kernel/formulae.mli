module type AtomType = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val negation: t -> t
  val print_in_fmt: Format.formatter -> t -> unit
  val toString: t -> string
  val id: t -> int
  val hash: t -> int
  val clear: unit->unit
end

type ('a,'b) form =
  | Lit of 'b
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a

module type FormulaImplem = sig
  type t
  type lit
  val reveal : t -> (t,lit) form
  val build : (t,lit) form -> t
end

module PrintableFormula(Atom: AtomType)(F: FormulaImplem with type lit = Atom.t) : sig
  type t = F.t
  val toString : F.t -> string
  val negation : F.t -> F.t
(*  val lit : bool * string * Term.t list -> F.t *)
  val andN : F.t * F.t -> F.t
  val andP : F.t * F.t -> F.t
  val orN : F.t * F.t -> F.t
  val orP : F.t * F.t -> F.t
end

