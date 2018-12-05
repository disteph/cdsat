include HardCaml.Comb.S with type t = Signal.t

(* is the constant true signal (implies width = 1) *)
val isT : t -> bool

(* is the constant false signal (implies width = 1) *)
val isF : t -> bool

(* Represent the same signal *)
val equal : t -> t -> bool
val compare : t -> t -> int

val hash : t -> int
val hash_fold_t : t Ppx_hash_lib.Std.Hash.folder

val pp : Format.formatter -> t -> unit
val show : t -> string
