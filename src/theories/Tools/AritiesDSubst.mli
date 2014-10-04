(*******************************)
(* Standard Arities and DSubst *)
(*******************************)

open Kernel.Interfaces_I

(* Basic module for arities *)

module StandardArity : sig 

  module IntMap: Map.S with type key = int

  type eigen = int
  type meta  = int
  type t = private {
    next_eigen : int;
    next_meta  : int;
    eigen_dependencies : int IntMap.t;
    meta_dependencies  : int IntMap.t;
  }

  val init     : t
  val newEigen : t -> eigen*t
  val newMeta  : t -> meta*t
  val prefix  : t -> t -> bool
  val print_in_fmt: Format.formatter -> t -> unit

  val liftE: t -> t
  val liftM: t -> t
  val projE: t -> t
  val projM: t -> t
  val print_in_fmtEM : Format.formatter -> t -> unit
  val print_in_fmtME : Format.formatter -> t -> unit

end

(* Basic module for delayed substitutions *)

module StandardDSubst : sig

  include DSubstType with module Arity = StandardArity
  type freeVar = Eigen of Arity.eigen | Meta of Arity.meta 
  val get: int -> t -> freeVar

end
