exception NotConstantSig

include HardCaml.Transform.CombBaseGates

val bit : int -> t -> BDD.t

(* Do not use, rather use isT and isF of Circuit *)
val isSyntacticT : t -> bool

(* injects a bitvector value as a constant signal *)
val cast : Top.Bv_value.t -> t

(* identity signal of a given width *)
val id : int -> t
