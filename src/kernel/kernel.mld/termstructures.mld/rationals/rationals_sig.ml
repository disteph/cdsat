open General
open Patricia_tools

open Top
open Specs

(* Type for the nature of a rational predicate *)
type nature = Lt | Le | Eq | NEq | Term | Other

(* Type of maps from rational variables to rational coefficients *)
type 'data varmap = ('data termF, Q.t, int, int, EmptyInfo.infos*[`NoHCons]) Patricia.poly

type 'data t = { scaling : Q.t; (* A scaling factor, so that multiplication by a constant
                                  does not necessitate a traversal of the map *)
                 coeffs : 'data varmap;  (* The map from variables to coefficients *)
                 constant: Q.t;      (* The constant term *)
                 nature : nature }   (* The predicate *)

module type API = sig
  type datatype
  type tset
end
