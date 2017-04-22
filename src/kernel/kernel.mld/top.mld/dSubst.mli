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

open Variables

exception DSubst of string

type 'l t = ('l*World.t) list [@@deriving eq, hash, show]

val get_arity : _ t -> World.t
val get: (Format.formatter -> 'l -> unit) -> int -> 'l t -> 'l*World.t
