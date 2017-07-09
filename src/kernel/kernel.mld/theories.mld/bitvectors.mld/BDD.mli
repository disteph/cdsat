type var = int
type man = MLBDD.man
type t = MLBDD.t
type support = MLBDD.support
val init : ?cache:int -> unit -> man
val clear : man -> unit
val manager : t -> man
val support : t -> support
val list_of_support : support -> int list
val support_of_list : int list -> support
val string_of_support : support -> string
val is_true : t -> bool
val is_false : t -> bool
val equal : t -> t -> bool
val id : t -> int
val hash : t -> int
val dtrue : man -> t
val dfalse : man -> t
val ithvar : man -> var -> t
val dnot : t -> t
val dand : t -> t -> t
val dor : t -> t -> t
val nand : t -> t -> t
val xor : t -> t -> t
val nxor : t -> t -> t
val eq : t -> t -> t
val ite : t -> var -> t -> t
val imply : t -> t -> t
val exists : support -> t -> t
val forall : support -> t -> t
val cofactor : var -> t -> t * t
val permute : var array -> t -> t
val permutef : (var -> var) -> t -> t
type 'a e = 'a MLBDD.e = False | True | Not of 'a | If of 'a * var * 'a
val inspect : t -> t e
type 'a b = 'a MLBDD.b = BFalse | BTrue | BIf of 'a * var * 'a
val inspectb : t -> t b
val fold : ('r e -> 'r) -> t -> 'r
val foldb : ('r b -> 'r) -> t -> 'r
val sat : t -> (bool * var) list option
val allsat : t -> (bool * var) list list
val itersat : ((bool * var) list -> unit) -> t -> unit
val prime : t -> (bool * var) list option
val allprime : t -> (bool * var) list list
val iterprime : ((bool * var) list -> unit) -> t -> unit
val to_stringb : t -> string
module Raw = MLBDD.Raw
module type WHS =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val replace : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val clear : 'a t -> unit
    val length : 'a t -> int
  end
module WeakHash = MLBDD.WeakHash

(* Printing a BDD in Latex.
Takes as parameter the printing function for a BDD node,
which in the implementation is just an int *)
                    
val pp : (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit
                                                                         
val to_string : (Format.formatter -> int -> unit) -> t -> string

val man : MLBDD.man
