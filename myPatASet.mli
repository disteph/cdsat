open Collection
open Memoisation
open Patricia

module type MyPatCollect =
sig
  module CI  : CollectImplem
  module Ext : Memoisation.CollectImplemExt with type e = CI.e and type t = CI.t

  type e = CI.e
  type t = CI.t
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
  val toString : t -> string
  val compare : t -> t -> int
  val compareE : e -> e -> int
  val min : t -> e option
  val diff : t -> t -> t
  val first_diff : t -> t -> e option * bool
  val choose : t -> e
  val clear: unit->unit
  val cardinal: t->int
  val find_su :
    (common -> common -> branching option -> ('a, branching) almost) ->
    bool ->
    (branching -> bool) ->
    ('b -> bool) ->
    (e -> 'c) ->
    'b ->
    (e -> branching -> 'b) ->
    ('b -> 'b -> 'b) ->
    common ->
    t -> 
    ('c, 'b) sum
end

module MyPat(UT:sig
	       include Intern
	       val compare : keys->keys->int
	       val toString: keys->string
	       val tString: ((common -> string)*(branching->string)) option
	     end):MyPatCollect with type CI.e = UT.keys and type common=UT.common and type branching = UT.branching

module MyPatriciaCollectImplem(M : sig
           type t
           val id : t -> int
           val compare : t -> t -> int
           val toString : t -> string
         end):MyPatCollect with type CI.e = M.t

module MyPatA : 
  sig
    module CI  : ACollectImplem with type e = Formulae.Atom.t
    module Ext : CollectImplemExt with type e = Formulae.Atom.t and type t = CI.t

    type e = Formulae.Atom.t
    type t = CI.t
    val hash : t -> int
    val equal : t -> t -> bool
    val empty : t
    val is_empty : t -> bool
    val union : t -> t -> t
    val inter : t -> t -> t
    val is_in : Formulae.Atom.t -> t -> bool
    val add : Formulae.Atom.t -> t -> t
    val remove : Formulae.Atom.t -> t -> t
    val next : t -> Formulae.Atom.t * t
    val toString : t -> string
    val filter : bool -> Formulae.Atom.Predicates.t -> t -> t
    val compare : t -> t -> int
    val compareE : Formulae.Atom.t -> Formulae.Atom.t -> int
    val min : t -> Formulae.Atom.t option
    val diff : t -> t -> t
    val first_diff : t -> t -> Formulae.Atom.t option * bool
    val clear: unit->unit
    val id: t-> int
  end
