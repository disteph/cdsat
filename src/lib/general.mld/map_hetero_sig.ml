module type Key = sig
  (* Implementation of keys and how to compute them.
    Typically for a HConsed keys type, common = int *)

  type _ t
  val compare : 'a t -> 'b t -> ('a,'b) Poly.ord
  
  type common
  val tag : _ t -> common

  (* Branching is the type of data used for discriminating the keys (themselves
    represented in common via tag) *) 

  type branching
  val bcompare : branching Compare.t

  (* Check discriminates its first argument over second *)
  val check : common -> branching -> bool

  (* Given two elements of common, disagree outputs: their common part, the
    first branching data that discriminates them a boolean saying whether that
    data was in the first [true] or second [false] argument *)
  val disagree : common -> common -> common * branching * bool

  (* Checks whether the first argument is compatible with the second up to some
    branching data. Should output true if the first two arguments are equal *)
  val match_prefix : common -> common -> branching -> bool
end

module type ArgNH = sig

  (* Domain of the map (keys) *)
  include Key

  (* Co-domain of the map (values) *)
  type _ values

end

module type ArgH = sig
  include ArgNH
  (* Do you want the patricia trees hconsed? if so you should provide
    an equal function for values, and hash functions for keys and values *) 
  val hash_fold_t : _ t Hash.folder
  val hash_fold_values : 'a t -> 'a values Hash.folder
  val equal_values: 'a t -> 'a values Equal.t
end


module type S = sig
  type 'a keys
  type 'a values
  type common
  type branching
  type hcons

  type t

  val is_empty : t -> bool
  val mem      : 'a keys -> t -> bool
  val find     : 'a keys -> t -> 'a values
  val cardinal : t -> int
  val empty    : t
  val singleton: 'a keys -> 'a values -> t

  val remove : 'a keys -> t -> t
  val add    : 'a keys -> ('a values option -> 'a values) -> t -> t

  type ('seed,'b) fold = { fold : 'p. 'p keys -> 'p values -> 'seed -> 'b} [@@unboxed]
  val fold : ('a,'a) fold -> t -> 'a -> 'a
  val fold_monad :
    return:('a -> 'b)
    -> bind:((t -> 'a -> 'b) -> t -> 'b -> 'b)
    -> ('a,'b) fold
    -> t -> 'a -> 'b

  module Fold2 : sig
    type 'b combine = {
      combineFF : t -> t -> 'b -> 'b;
      combineEF : t -> 'b -> 'b;
      combineFE : t -> 'b -> 'b
    }
    val make_combine :
      (reccall : (t -> t -> 'a -> 'b) -> (t -> t -> 'b -> 'b))
      -> reccall : (t -> t -> 'a -> 'b)
      -> 'b combine
    type nonrec ('a,'b) t = {
        sameleaf  : 'p. 'p keys -> 'p values -> 'p values -> 'a -> 'b;
        emptyfull : t -> 'a -> 'b;
        fullempty : t -> 'a -> 'b;
        combine   : reccall : (t -> t -> 'a -> 'b) -> 'b combine
      }
  end

  val fold2 : ?equal:(t -> 'a -> 'b)
    -> ('a,'b) Fold2.t
    -> t
    -> t
    -> 'a -> 'b

  module Merge : sig
    type nonrec 'a t = {
        sameleaf  : 'p. 'p keys -> 'p values -> 'p values -> 'a;
        emptyfull : t -> 'a;
        fullempty : t -> 'a;
        combine   : 'a -> 'a -> 'a
      }
  end
                   
  val merge  : ?equal:(t -> 'a) -> 'a Merge.t -> t -> t -> 'a
  type union = {union : 'p. 'p keys -> 'p values -> 'p values -> 'p values} [@@unboxed]
  val union  : union -> t -> t -> t
  type inter = {inter : 'p. 'p keys -> 'p values -> 'p values -> 'p values} [@@unboxed]
  val inter  : inter -> t -> t -> t
  type diff  = {diff : 'p. 'p keys -> 'p values -> 'p values -> t} [@@unboxed]
  val diff   : diff -> t -> t -> t
  type subset = {subset : 'p. 'p values -> 'p values -> bool} [@@unboxed]
  val subset : subset  -> t -> t -> bool
  (* val make : ('a -> values option -> values) -> (keys * 'a) list -> t *)
  (* val elements : t -> (keys * values) list *)

  (* val print_tree_in_fmt:
   *   ?common      :(Format.formatter -> common -> unit)
   *   -> ?branching:(Format.formatter -> branching -> unit)
   *   -> (Format.formatter -> (keys*values) -> unit)
   *   -> Format.formatter -> t -> unit
  *)
  type print_pair = {print_pair: 'p . ('p keys * 'p values) Format.printer} [@@unboxed]
  val print_in_fmt:
    ?tree:(common Format.printer * branching Format.printer)
    -> ?sep:string -> ?wrap:string*string
    -> print_pair
    -> t Format.printer
end

module type S_H = sig
  include S with type hcons = [`HCons]
  val equal    : t Equal.t
  val hash_fold_t : t Hash.folder
  val hash     : t Hash.t
  val compare  : t Compare.t
  val id       : t -> int
  val clear    : unit -> unit
end

module type S_NH = S with type hcons = [`NoHCons]
