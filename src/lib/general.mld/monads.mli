include module type of Monads_sig
    
module IdMon : Monad with type 'a t = 'a

module ContMonad(R:sig type t end) : Monad with type 'a t = ('a -> R.t) -> R.t

module StateMonad(State:sig type t end) : Monad with type 'a t = State.t -> 'a * State.t

module Make_Let(M:Monad) : sig
  module Let_syntax : Let_syntax with type 'a t = 'a M.t
end  
