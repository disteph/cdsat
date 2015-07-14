(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module EmptyData : sig
  type t
  val build : int -> 'a -> t
end

module type PolyS = sig
  type ('t,'a) initial
  type ('a,'data) generic
  type ('a,'data) g_revealed = (('a,'data) generic,'a) initial
  val reveal : ('a,'data) generic -> ('a,'data) g_revealed
  val id     : ('a,'data) generic -> int
  val data   : ('a,'data) generic -> 'data
  val compare: ('a,'data) generic -> ('a,'data) generic -> int
end

module MakePoly
  (M: sig 
    type ('t,'a) t
    val equal: ('t->'t->bool) -> ('a->'a->bool) -> (('t,'a) t -> ('t,'a) t -> bool) 
    val hash : ('t->int) -> ('a->int) -> (('t,'a) t -> int)    
  end) : sig

    include PolyS with type ('t,'a) initial = ('t,'a) M.t

    module InitData
      (Par: Hashtbl.HashedType)
      (Data: sig
        type t
        val build : int -> (Par.t,t) g_revealed -> t
      end)
      : sig
        type t = (Par.t,Data.t) generic
        type revealed = (Par.t,Data.t) g_revealed
        val build : revealed -> t
        val clear : unit -> unit
      end

    module Init(Par: Hashtbl.HashedType)
      : sig
        type t = (Par.t,unit) generic
        type revealed = (Par.t,unit) g_revealed
        val build : revealed -> t
        val clear : unit -> unit
      end

  end

module type S = sig
  type 't initial
  type 'data generic
  type 'data g_revealed = 'data generic initial
  val reveal : 'data generic -> 'data g_revealed
  val id     : 'data generic -> int
  val data   : 'data generic -> 'data
  val compare: 'data generic -> 'data generic -> int
end

module Make
  (M: sig 
    type 't t
    val equal: ('t->'t->bool) -> 't t -> 't t -> bool
    val hash : ('t->int) -> 't t -> int
  end) : sig

    include S with type 't initial = 't M.t

    module InitData
      (Data: sig
        type t
        val build : int -> t g_revealed -> t
      end)
      : sig
        type t = Data.t generic
        type revealed = Data.t g_revealed
        val build : revealed -> t
        val clear : unit -> unit
      end
      
    module Init : sig
      type t = unit generic
      type revealed = unit g_revealed
      val build : revealed -> t
      val clear : unit -> unit
    end

  end
