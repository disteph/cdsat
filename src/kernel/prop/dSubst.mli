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

type t
val id: t -> int
val print_in_fmt: Format.formatter -> t -> unit
val clear: unit->unit
val compare : t -> t -> int

val init      : t
val bind2FV: (World.FreeVar.t*World.t) -> t -> t
val get_arity : t -> World.t
val get: int -> t -> World.FreeVar.t*World.t
