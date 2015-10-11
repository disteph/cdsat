(******************)
(* Specifications *)
(******************)

open Format

open Interfaces_basic
open Basic
open Variables
open Messages

exception ModelError of string

(* Useful abbreviations for module types *)

module type TermF = Terms.S with type leaf := FreeVar.t

(* Useful abbreviation for term type *)

type 'a termF = (FreeVar.t,'a) Terms.term

let get_sort t = match Terms.reveal t with
  | Terms.V fv      -> FreeVar.get_sort fv
  | Terms.C(symb,_) -> let so,_ = Symbols.arity symb in so

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

module type DataType = Terms.DataType with type leaf := FreeVar.t

module Pairing(B1: DataType)(B2: DataType)
  : (DataType with type t = B1.t*B2.t) =
struct
  type t = B1.t*B2.t
  let bC tag symb args = 
    (B1.bC tag symb (List.map fst args),
     B2.bC tag symb (List.map snd args))
  let bV tag v = (B1.bV tag v, B2.bV tag v)
end

module type GTheoryDSType = sig
  module Term : TermF
  module TSet : Collection with type e = Term.t
end

module type ProofType = sig
  type t
  type seq
  val zero : seq->t
  val one  : seq->t->t
  val two  : seq->t->t->t
  val print_in_fmt: formatter -> t -> unit
end

module type SlotMachine = sig
  type newoutput
  type tset
  val treated  : unit -> tset
  val add      : tset option -> newoutput
  val normalise: tset -> newoutput
  val clone    : unit -> newoutput
end

type ('sign,'tset) slot_machine
    = (module SlotMachine with type newoutput = ('sign,'tset) output and type tset = 'tset)

and (_,_) output = Output:
  ('sign,'tset,'msg) thsays option
  * ('sign,'tset) slot_machine
  -> ('sign,'tset) output

let fail_state (type sign) (type ts) : (sign,ts) slot_machine = (module struct 
  type newoutput = (sign,ts) output
  type tset = ts
  let treated _ = failwith "Are you dumb? I already told you it was provable"
  let add     _ = failwith "Are you dumb? I already told you it was provable"
  let normalise _ = failwith "Are you dumb? I already told you it was provable"
  let clone   _ = failwith "Are you dumb? I already told you it was provable"
end)
