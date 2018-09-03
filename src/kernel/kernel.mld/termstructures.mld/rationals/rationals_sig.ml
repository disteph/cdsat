open General
open Patricia_tools

open Top.Terms

(* Type for the nature of a rational predicate *)
type nature = Lt | Le | Eq | NEq | Term | Other

(* Type of maps from rational variables to rational coefficients *)
type varmap = (Term.t, Q.t, int, int, EmptyInfo.infos*[`NoHCons]) Patricia.poly

type t = { scaling : Q.t;     (* A scaling factor, so that multiplication by a constant
                                 does not necessitate a traversal of the map *)
           coeffs  : varmap;  (* The map from variables to coefficients *)
           constant: Q.t;     (* The constant term *)
           nature  : nature } (* The predicate *)
