include HardCaml.Comb.S
val isT : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val hash_fold_t : t Ppx_hash_lib.Std.Hash.folder
val pp : Format.formatter -> t -> unit
val show : t -> string
val name : string
