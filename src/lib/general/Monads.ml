module type MonadType = sig
  type 'a t 
  val return : 'a -> 'a t
  val bind   : ('a -> 'b t) -> 'a t -> 'b t
end

module IdMon = (struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end : MonadType with type 'a t = 'a)

module ContMonad(R:sig type t end)
  = (struct
    type 'a t = ('a -> R.t) -> R.t
    let return a = fun f -> f a
    let bind (f: 'a -> 'b t) (a: 'a t)
        = fun (g:'b -> R.t) -> a (fun x -> f x g)
  end:MonadType with type 'a t = ('a -> R.t) -> R.t)
