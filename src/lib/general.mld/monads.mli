include module type of Monads_sig
    
module IdMon : Monad with type 'a t = 'a

module ContMonad(R:sig type t end) : Monad with type 'a t = ('a -> R.t) -> R.t

module StateMonad(State:sig type t end) : Monad with type 'a t = State.t -> 'a * State.t

module Make_Let(M:Monad) : sig
  open M      
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map    : 'a t -> f:('a -> 'b) -> 'b t
    val bind   : 'a t -> f:('a -> 'b t) -> 'b t
    val both   : 'a t -> 'b t -> ('a * 'b) t
  end
end  
