module type Monad = sig
  type 'a t 
  val return : 'a -> 'a t
  val bind   : ('a -> 'b t) -> 'a t -> 'b t
end
