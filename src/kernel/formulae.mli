open Interfaces

module PrintableFormula(Atom: AtomType)(F: FormulaImplem with type lit = Atom.t) 
  : PrintableFormulaType with type lit = Atom.t and type t=F.t
