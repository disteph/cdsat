open General.Monads
  
include module type of TwoWatchedLits_sig

module StdMonad(Fixed: sig type t end) : Monad with type 'a t = Fixed.t -> 'a

module Make(C : Config) : S with module C := C

val pick_another_make
    : is_empty:('varset -> bool) ->
      mem     :('var -> 'varset -> bool) ->
      next    :('varset -> 'var * 'varset) ->
      remove  :('var -> 'varset -> 'varset) ->
      'varset ->
      int ->
      'var list ->
      'var list
