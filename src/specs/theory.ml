(**********************************************************)
(* This file contains the specification of theory modules *)
(**********************************************************)

open Kernel
open Basic
open Interfaces_basic
open Formulae
open Interfaces_theory

(* Internal representation of objects in the theory module, used
   during parsing. 
   Similar to the definition of a model structure in logic:

   Type t is the main type, in which all objects are constructed (=
   support set of a model structure)

   Every symbol is interpreted as a function taking a certain number
   of arguments in t, and producing an object in t.

   Type leaf is the type of objects implementing "variables"; the type
   will be imposed at some point as the type of bound variables, or
   the type of eigen-variables + meta-variables. Should be castable in
   type t with function leaf.

   Like any other objects, propositions are implemented as inhabitants
   of type t, but there is a type form to which they can be exported;
   the type will be imposed as the type of formulae (see later).
   Function toForm is expected to raise a ModelError if called on an
   inhabitant of t that is not representing a proposition.
*)

exception ModelError of string

module type ForParsingType = sig
  type t
  val semantic : Symbol.t  -> t list -> t
  type leaf
  val leaf     : leaf -> t
end

(* Data-structures that a theory should provide.

   names indicates a list of names for the theory

   sugPlugin allows the theory to specify a plugin that is well-suited
   for it.

   Module Atom commits to a particular type leaf for these leaves, and
   the rest of the module provides the basic functions for
   manipulating atoms (see Kernel.Interfaces_I for the definition of
   AtomType)

   Module ForParsing provides the data-structures used at parsing time as
   described further up. For that, the theory is provided with the
   data-structure for formulae.

   Using that data-structure, the theory can also provide a list of
   illusrative examples. Each example comes with a boolean saying
   whether the formula is provable (true) or not (false).
*)

module type StructType = sig

  val names    : string list
  val sugPlugin: string option

  module Atom : AtomType

  module ForParsing(F: Kernel.Formulae.Formula.S with type lit = Atom.t) :
  sig
    include ForParsingType with type leaf := IntSort.t
    val toForm : t -> F.t
    val examples : ((unit->F.t)*bool) list
  end
    
end

(* Finally, complete module type for a theory.

   type 'leaf atom is the type family discussed above

   Every component of StructType, as described above, is required.

   Additionally, the theory must provide the data-structure for the
   constraints that its consistency functions will produces (as
   specified in Kernel.Interfaces_I).

   Finally, the theory provides its favorite implementation for sets
   of atoms, and the two consistency functions. It does so for a
   notion of atom (EAtom.t) that is slightly richer than the one it
   provided (Atom.t), and that is given as an argument module EAtom.
   This argument module provides a negation function, and can project
   any rich atom to a theory atom. It also provides a function is_meta
   that the theory can use to test if a leaf is a meta-variable or an
   eigen-variable (which is of course crucial for the consistency
   functions).

*)

module type Type = sig

  include StructType
  
  module Constraint : ConstraintType

  module Consistency(EAtom : sig
    type t
    val proj: t -> Atom.t
    val negation: t -> t
  end): sig
    module ASet : CollectTrusted with type e = EAtom.t
    val consistency     :            ASet.t -> (ASet.t,Constraint.t) stream
    val goal_consistency: EAtom.t -> ASet.t -> (ASet.t,Constraint.t) stream
  end

end
