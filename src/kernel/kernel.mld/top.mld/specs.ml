(******************)
(* Specifications *)
(******************)

open Format

open Interfaces_basic
open Basic
open Variables

exception ModelError of string

(* Useful abbreviations for module types *)

module type TermF = Terms.S with type leaf := FreeVar.t
module type DataType = Terms.DataType with type leaf := FreeVar.t

(* Useful abbreviation for term type *)

type 'a termF = (FreeVar.t,'a) Terms.term

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
  val bC: Symbols.t -> t list -> t
  val bV: IntSort.t -> t
end

(* This module type is for implementations of the global datastructures
   for the combination of theory modules *)

module type GTheoryDSType = sig
  module Term : TermF
  module Value : PH
  module Assign : Collection with type e = Term.t
end

(* Extension thereof,
   that adds interfacing functions with theory-specific types for terms and values.
     proj can project the global term datatype into the theory-specific one ts
     proj_opt, if the theory module has values, offers an injection (resp. a projection)
   from (resp. to) theory-specific values to (resp. from) global values (the projection
   ends in an option type since a global value may not contain a value for this theory).
*)

type _ has_values  = private HV
type has_no_values = private HNV
type ('a,'b) conv = {
    inj : 'a -> 'b;
    proj: 'b -> 'a option
  }
                               
type (_,_) proj_opt =
  | HasVproj :  ('v,'gv) conv -> ('gv,'v has_values) proj_opt
  | HasNoVproj : (_,has_no_values) proj_opt

module type DSproj = sig
  include GTheoryDSType
  type ts
  val proj: Term.datatype -> ts
  type values
  val proj_opt: (Value.t,values) proj_opt
end

(* type version of the above *)
type ('ts,'v,'gts,'gv,'assign) dsProj
  = (module DSproj with type ts = 'ts
                    and type values = 'v
                    and type Term.datatype = 'gts
                    and type Value.t = 'gv
                    and type Assign.t  = 'assign)
                       
(* Module type for proofs in Prop module*)
                       
module type ProofType = sig
  type t [@@deriving show]
  type seq
  val zero : seq->t
  val one  : seq->t->t
  val two  : seq->t->t->t
end

(* Standard API that a theory module, kernel-side, may offer to the plugins piloting it. *)
                          
module type SlotMachine = sig
  type newoutput
  type tset
  val add      : tset option -> newoutput
  val clone    : unit -> newoutput
  val normalise: tset -> newoutput
  val suicide  : ('sign,tset,Messages.unsat) Messages.message -> unit
end

type ('sign,'tset) slot_machine
  = (module SlotMachine with type newoutput = ('sign,'tset) output
                         and type tset = 'tset)
      
 and (_,_) output =
   Output:
     ('sign,'tset,_) Messages.message option
   * ('sign,'tset) slot_machine
   -> ('sign,'tset) output
        
