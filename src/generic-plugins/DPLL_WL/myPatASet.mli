open Lib
open Kernel

open Interfaces
open Patricia
open Sums

module type MyPatCollect = sig

  type e
  type t
  type common
  type branching
  val is_empty : t -> bool
  val is_in : e -> t -> bool
  val empty : t
  val add   : e -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val remove : e -> t -> t
  val hash : t -> int
  val equal : t -> t -> bool
  val next : t -> e * t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString : t -> string
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val sub  : bool->t->t->e option->(unit,e) almost
  val first_diff : t -> t -> e option * bool
  val iter : (e->unit)->t->unit
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

module MyPat(UT:sig
	       include Intern
	       val compare : keys->keys->int
	       val toString: keys->string
	       val tString: ((common -> string)*(branching->string)) option
	     end):MyPatCollect with type e = UT.keys and type common=UT.common and type branching = UT.branching

module MyPatriciaCollectImplem(M : sig
           type t
           val id : t -> int
           val compare : t -> t -> int
           val toString : t -> string
         end):MyPatCollect with type e = M.t

module MyPatA(Atom:AtomType) : sig

  type e = Atom.t
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val empty : t
  val is_empty : t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val latest: t -> e option
  val choose : t -> Atom.t
  val is_in : Atom.t -> t -> bool
  val add : Atom.t -> t -> t
  val remove : Atom.t -> t -> t
  val next : t -> Atom.t * t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString : t -> string
(*  val filter : bool -> Atom.Predicates.t -> t -> t *)
  val compare : t -> t -> int
  val compareE : Atom.t -> Atom.t -> int
  val sub      : bool->t->t->e option->(unit,e) almost
  val first_diff : t -> t -> Atom.t option * bool
  val clear: unit->unit
  val id: t-> int
  val cardinal: t->int
  val negations: t->t
end
