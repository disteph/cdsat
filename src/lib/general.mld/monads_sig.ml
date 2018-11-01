module type Monad = sig
  type 'a t 
  val return : 'a -> 'a t
  val bind   : ('a -> 'b t) -> 'a t -> 'b t
end

module type Let_syntax = sig
  type 'a t
  val return : 'a -> 'a t
  val map    : 'a t -> f:('a -> 'b) -> 'b t
  val bind   : 'a t -> f:('a -> 'b t) -> 'b t
  val both   : 'a t -> 'b t -> ('a * 'b) t
end
