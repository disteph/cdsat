(******************)
(* Specifications *)
(******************)

open Format

open General
       
open Interfaces_basic
open Basic
open Variables

(* Abbreviations *)

module type Term = Terms.S with type leaf = FreeVar.t

type 'd termF = (FreeVar.t,'d) Terms.termF

type ('t,'v) sassign = 't termF * 'v Values.t

module type DataType = Terms.DataType with type leaf := FreeVar.t

exception ModelError of string


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
  val bV: BoundVar.t -> t
end


(* Module of constraints that meta-variables may be subject
   to. Constraints are produced when closing a branch, and are
   propagated through the other open branches of the proof.
 *)

module type Constraint = sig
  type t [@@deriving ord,show]
  val topconstraint:t
  val proj : t -> t
  val lift : World.t -> t -> t
  val meet : t -> t -> t option
end

(* This module type is for implementations of the global datastructures
   for the combination of theory modules *)

module type CValue = sig
  type value
  type t [@@deriving eq,ord,show,hash]
  val none: Sorts.t -> t
  val inj : value Values.t -> t
  val merge : t -> t -> (value Values.t*value Values.t,t) Sums.sum
end
                       
module type GlobalDS = sig
  module Term   : Term
  module Value  : PH
  module CValue : CValue with type value := Value.t
  module Assign : Assign with type term = Term.t
                          and type v = Value.t Values.t
  val makes_sense : Term.t -> World.t -> bool
end

(* type version of the above *)

type ('gts,'gv,'cv,'assign) globalDS
  = (module GlobalDS with type Term.datatype = 'gts
                      and type Value.t   = 'gv
                      and type CValue.t  = (bool option,'cv)Sums.sum
                      and type Assign.t  = 'assign)

                         
(* Extension thereof,
   that adds interfacing functions with theory-specific types for terms and values.
     proj can project the global term datatype into the theory-specific one ts
     proj_opt, if the theory module has values, offers an injection (resp. a projection)
   from (resp. to) theory-specific values to (resp. from) global values (the projection
   ends in an option type since a global value may not contain a value for this theory).
*)

type _ has_values  = private HV
type has_no_values = private HNV

type (_,_) inj_opt =
  | HasVinj :  ('v -> 'gv) -> ('v has_values,'gv) inj_opt
  | HasNoVinj : (has_no_values,_) inj_opt

module type DSproj = sig
  include GlobalDS
  type ts
  val proj: Term.datatype -> ts
  type values
  val vinj: (values,Value.t) inj_opt
end

(* type version of the above *)
type ('ts,'v,'gts,'gv,'assign) dsProj
  = (module DSproj with type ts       = 'ts
                    and type values   = 'v
                    and type Term.datatype = 'gts
                    and type Value.t  = 'gv
                    and type Assign.t = 'assign)
                       
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
  type assign
  type sassign
  val add      : sassign option -> newoutput
  val clone    : unit -> newoutput
  val suicide  : assign -> unit
end

type ('sign,'assign,'sassign) slot_machine
  = (module SlotMachine with type newoutput = ('sign,'assign,'sassign) output
                         and type assign = 'assign
                         and type sassign = 'sassign)
      
 and (_,_,_) output =
   Output:
     ('sign,'ds,_) Messages.message option
   * ('sign,'assign,'sassign) slot_machine
   -> ('sign,'assign,'sassign) output
        
