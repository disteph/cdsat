(*************************)
(* Delayed Substitutions *)
(*************************)

(* Module of Delayed Substitutions.

   Kernel will not perfom a substitution every time it breaks a ForAll
   or a ThereExists; instead, it records in a datastructure called
   "Delayed substitution" what the bound variable is supposed to be
   substituted by.

   Namely, bound variables will be "substituted by" eigenvariables or
   meta-variables (depending on the quantifier), hence the two
   functions bind2eigen and bind2meta.
*)

open Top
open Interfaces_basic
open Variables

include PHCons
          
val init      : t
val bind2FV: (FreeVar.t*World.t) -> t -> t
val get_arity : t -> World.t
val get: int -> t -> FreeVar.t*World.t
