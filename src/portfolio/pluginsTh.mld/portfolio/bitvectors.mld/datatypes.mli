open Format
       
open General
open Patricia
open Patricia_interfaces

open Kernel
open Export
open Theories.Bitvectors
open Top.Messages
       
open Tools
       
module Make(DS: GlobalImplem)
         (K: MyTheory.API with type assign = DS.Assign.t
                           and type termdata= DS.Term.datatype
                           and type tset   = DS.TSet.t ) : sig

  open DS

  type datatypes = Term.datatype * Value.t * Assign.t * TSet.t

  module Domain : PatMap
         with type keys   = int
          and type values = Assign.t Range.t
          and type common = int
          and type branching = int
          and type ('v,'i) param = (int,'v,int,int,'i) poly

end
