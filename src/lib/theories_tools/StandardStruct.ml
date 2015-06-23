open Format

open Kernel
open Interfaces_basic
open Interfaces_theory
open General

module StandardDSData
  (Leaf : PHCons)
  (Data : Semantic with type leaf := Leaf.t) =
struct

  module Atom = AtomDef.Make(Leaf)(Data)

  module ForParsingWOEx(F: Kernel.Formulae.Formula.S with type lit = Atom.t)
      = struct

        type t =
        | TermI : Atom.Term.t -> t
        | PropI : F.t -> t

        type leaf = Leaf.t
        let leaf v = TermI(Atom.Term.bV v)

        let toForm = function
          | PropI f -> f
          | _       -> raise (Theory.ModelError "ModelError: trying to convert into a formula an expression that clearly is not one")

        let toTerm = function
          | TermI t -> t
          | _       -> raise (Theory.ModelError "ModelError: trying to convert into a term an expression that clearly is not one")


        module PropIntern = Prop.Semantics.Intern(F)

        let lit (b, f, tl) = F.lit(Atom.build(b, f, tl))

        let semantic symb =
          let (o,_) = Symbol.arity symb in
          match o with
	  | Sorts.Prop -> fun l -> 
            PropI (
              try PropIntern.semantic symb (List.map toForm l)
              with Theory.ModelError _
                -> lit(true,symb,List.map toTerm l)
            )
	  | _ -> fun l -> 
	    TermI (Atom.Term.bC symb (List.map toTerm l))

        let examples = []

      end
end


module StandardDS(Leaf : PHCons) = StandardDSData(Leaf)(Basic.EmptyData(Leaf))
