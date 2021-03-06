open General
open Patricia
open Patricia_interfaces
open Top.Basic
       
module VarMap : PatMap
       with type keys   = int
        and type values = Q.t
        and type ('v,'i) param = (int,'v,int,int,'i) poly 

type nature = Lt | Le | Eq | NEq | Term | Other

type t' = { scaling : Q.t; (* A scaling factor, so that multiplication by a constant
                              does not necessitate a traversal of the map *)
            coeffs : VarMap.t;  (* The map from variables to coefficients *)
            constant: Q.t;      (* The constant term *)
            nature : nature }   (* The predicate *)            
            
include Top.Specs.DataType with type t = t'

val pp : (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit
