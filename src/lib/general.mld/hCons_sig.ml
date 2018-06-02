(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module type SHCons = sig
  type t [@@deriving eq,ord,hash]
  type revealed
  val id    : t -> int
  val build : revealed -> t
  val clear : unit -> unit
  (* val backindex: (int -> t,M.backindex) Goption.t *)
end

module type PolyArg = sig 
  type ('t,'a) t [@@deriving eq,hash]
  val name : string
end

module type PolyS = sig

  type ('t,'a) initial

  module G : sig
    type 'p t constraint 'p=_*_*_
    type 'p revealed = ('p t,'a) initial constraint 'p='a*_*_
    val reveal  : 'p t -> 'p revealed
    val data    : (_*'data*_) t -> 'data
  end

  module NoHCons : sig
    type 'p t        = ('a*'data*[`NoHCons]) G.t constraint 'p='a*'data
    type 'p revealed = ('a*'data*[`NoHCons]) G.revealed constraint 'p='a*'data
    val build   : ('a*'data) revealed -> 'data Lazy.t -> ('a*'data) t
  end

  module InitData
      (M   : PolyArg with type ('recurs,'a) t := ('recurs,'a) initial)
      (Par : sig type t [@@deriving eq,hash] end)
      (Data: sig
         type t
         val build : (Par.t*t*[`HCons]) G.t -> t
       end)
    : SHCons with type t        = (Par.t*Data.t*[`HCons]) G.t
              and type revealed = (Par.t*Data.t*[`HCons]) G.revealed

  module Init
      (M   : PolyArg with type ('recurs,'a) t := ('recurs,'a) initial)
      (Par: sig type t [@@deriving eq,hash] end)
    : SHCons with type t        = (Par.t*unit*[`HCons]) G.t
              and type revealed = (Par.t*unit*[`HCons]) G.revealed
end

module type Arg = sig 
  type 't t [@@deriving eq,hash]
  val name : string
end

module type S = sig

  type 't initial

  module G : sig
    type 'p t constraint 'p=_*_
    type 'p revealed = 'p t initial constraint 'p=_*_
    val reveal : 'p t -> 'p revealed
    val data   : ('data*_) t -> 'data
  end

  module NoHCons : sig
    type 'data t        = ('data*[`NoHCons]) G.t
    type 'data revealed = ('data*[`NoHCons]) G.revealed
    val build  : 'data revealed -> 'data Lazy.t -> 'data t
  end

  module InitData
      (M   : Arg with type 'a t := 'a initial)
      (Data: sig
         type t
         val build : (t*[`HCons]) G.t -> t
       end)
    : SHCons with type t        = (Data.t*[`HCons]) G.t
              and type revealed = (Data.t*[`HCons]) G.revealed

  module Init(M : Arg with type 'a t := 'a initial)
    : SHCons with type t        = (unit*[`HCons]) G.t
              and type revealed = (unit*[`HCons]) G.revealed

end

