(******************)
(* Specifications *)
(******************)

open Format

open Interfaces_basic
open Basic

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

module type ForParsing = sig
  type t
  val semantic : Symbol.t  -> t list -> t
  val leaf     : IntSort.t -> t
end

module type Semantic = sig
  type t
  val semantic : Symbol.t  -> (t list -> t) option
  val leaf     : IntSort.t -> t
end

(* Useful abbreviations for module types *)

module type Term = Terms.S with type leaf := IntSort.t

module type GTheoryDSType = sig
  module Term : Term
  module TSet : CollectTrusted with type e = Term.t
end
