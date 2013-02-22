open Interfaces

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

