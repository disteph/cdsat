exception NotConstantSig

include HardCaml.Transform.CombBaseGates

val bit : int -> t -> BDD.t

val isT : t -> bool

val cast : V.t -> t

val id : int -> t
