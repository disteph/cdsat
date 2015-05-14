(* Interface for the input of the Memoisation module and output of
myPatAset *)

open General.Sums

module type CollectImplemExt = sig
  include Kernel.Interfaces_basic.CollectExtra

  val mem    : e -> t -> bool
  val is_empty : t -> bool
  val next     : t -> e*t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a

    (* Comparison of collections *)
  val compare    : t->t->int

    (* Comparison of elements *)
  val compareE   : e->e->int

    (* sub false k1 k2 (Some limit)
       computes whether k1 is a subset of k2
       up to the element limit (excluded);
       replace (Some limit) with None if you want no limit.
       It answers Yes() or No.

       sub true... refines the answer No into the answer Almost(a)
       if k1 is almost a subset of k2, were it not for element a
       (necessarily smaller than the limit if there is one)
    *)
  val sub       : bool->t->t->e option->(unit,e) almost

    (* Computes the smallest element that is in one set 
       and not in the other *)
  val first_diff : t->t->(e option*bool)

end

module type MyPatCollect =
sig

  type e
  type t
  type common
  type branching

  val id    : t -> int
  (* val checktree : branching list -> t -> bool *)
  val is_empty : t -> bool
  val mem   : e -> t -> bool
  val empty : t
  val add   : e -> t -> t
  val union : t -> t -> t
  val subset : t -> t -> bool
  val inter : t -> t -> t
  val remove : e -> t -> t
  val hash : t -> int
  val equal : t -> t -> bool
  val next : t -> e * t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val print_in_fmt: Format.formatter -> t -> unit
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val sub  : bool->t->t->e option->(unit,e) almost
  val first_diff : t -> t -> e option * bool
  val iter : (e->unit)->t->unit
  val diff : t -> t -> t
  val fold : (e->'a->'a)->t-> 'a -> 'a
  val choose : t -> e
  val clear: unit->unit
  val cardinal: t->int
  val find_su :
    (e->'a) ->
    (e->branching->'b) ->
    'b ->
    ('b -> 'b -> 'b) ->
    (common -> common -> branching option -> ('c, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) ->
    common ->
    t -> 
    ('a, 'b) sum

end
