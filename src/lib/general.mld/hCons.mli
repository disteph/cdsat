(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module type OptionValue = sig
  type t
  type index
  val value: (t,index) Opt.gadt
end

module BackIndex: OptionValue with type index = Opt.some
module NoBackIndex: OptionValue with type index = Opt.none

module EmptyData : sig
  type t
  val build : _ -> t
end

module type PolyS = sig
  type ('t,'a) initial
  type ('a,'data) generic
  type ('a,'data) g_revealed = (('a,'data) generic,'a) initial
  val reveal : ('a,'data) generic -> ('a,'data) g_revealed
  val id     : ('a,'data) generic -> int
  val data   : ('a,'data) generic -> 'data
  (* val compare: ('a,'data) generic -> ('a,'data) generic -> int *)
end

module MakePoly(M: sig 
                    type ('t,'a) t [@@deriving eq, hash]
                    val name : string
                  end) : sig

  include PolyS with type ('t,'a) initial = ('t,'a) M.t

  module InitData(B:OptionValue)
           (Par:  sig type t [@@deriving eq, hash] end)
           (Data: sig
              type t
              val build : (Par.t,t) generic -> t
            end)
         : sig
    type t = (Par.t,Data.t) generic [@@deriving eq,hash,ord]
    type revealed = (Par.t,Data.t) g_revealed
    val build : revealed -> t
    val clear : unit -> unit
    val backindex: (int -> t,B.index) Opt.gadt
  end

  module Init(B:OptionValue)(Par: sig type t [@@deriving eq, hash] end)
         : sig
    type t = (Par.t,unit) generic [@@deriving eq,hash,ord]
    type revealed = (Par.t,unit) g_revealed
    val build : revealed -> t
    val clear : unit -> unit
    val backindex: (int -> t,B.index) Opt.gadt
  end

end

module type S = sig
  type 't initial
  type 'data generic
  type 'data g_revealed = 'data generic initial
  val reveal : 'data generic -> 'data g_revealed
  val id     : 'data generic -> int
  val data   : 'data generic -> 'data
end

(* module type BuiltS = sig
 *   type 't initial
 *   type data
 *   type t [@@deriving eq,hash,ord]
 *   val reveal : t -> t initial
 *   val id     : t -> int
 *   val data   : t -> data
 *   val built  : t initial -> t
 *   val clear  : unit -> unit
 * end *)

module Make(M: sig 
                type 't t [@@deriving eq, hash]
                val name : string
              end) : sig
  
  include S with type 't initial = 't M.t

  module InitData(B:OptionValue)
           (Data: sig
                type t
                val build : t generic -> t
              end)
         : sig
    type t = Data.t generic [@@deriving eq,hash,ord]
    type revealed = Data.t g_revealed
    val build : revealed -> t
    val clear : unit -> unit
    val backindex: (int -> t,B.index) Opt.gadt
  end
             
  module Init(B:OptionValue) : sig
    type t = unit generic [@@deriving eq,hash,ord]
    type revealed = unit g_revealed
    val build : revealed -> t
    val clear : unit -> unit
    val backindex: (int -> t,B.index) Opt.gadt
  end

end
