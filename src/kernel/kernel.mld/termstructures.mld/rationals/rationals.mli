open General.Patricia

open Top.Terms
       
module VarMap : sig
  include Map.S_NH with type keys   = Term.t
                          and type values = Q.t
                          and type common = int
                          and type branching = int
                          and type hcons = [ `NoHCons ]
  val pp : ?constant:Q.t -> ?scaling:Q.t -> unit -> t Format.printer
end

(* Type for the nature of a rational predicate *)
type nature = private Lt | Le | Eq | NEq | Term | Other

type t = private { scaling : Q.t;     (* A scaling factor, so that multiplication by a constant
                                         does not necessitate a traversal of the map *)
                   coeffs  : VarMap.t;  (* The map from variables to coefficients *)
                   constant: Q.t;     (* The constant term *)
                   nature  : nature } (* The predicate *)
[@@deriving fields]

val key : t Key.t
