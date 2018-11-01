include Monads_sig
    
module IdMon = (struct
  type 'a t = 'a
  let return a = a
  let bind (f: 'a -> 'b t) a = f a
end : Monad with type 'a t = 'a)

module ContMonad(R:sig type t end) = (struct
  type 'a t = ('a -> R.t) -> R.t
  let return a = fun f -> f a
  let bind (f: 'a -> 'b t) (a: 'a t)
    = fun (g:'b -> R.t) -> a (fun x -> f x g)
end : Monad with type 'a t = ('a -> R.t) -> R.t)

module StateMonad(State:sig type t end) = (struct
  type 'a t = State.t -> 'a * State.t
  let return a = fun state -> a,state
  let bind (f: 'a -> 'b t) (a: 'a t) = a >> fun (a,state) -> f a state
end : Monad with type 'a t = State.t -> 'a * State.t)

module Make_Let(M:Monad) = struct

  open M
      
  module Let_syntax = struct
    type 'a t = 'a M.t
    let return = return
    let both x y  = bind (fun x -> bind (fun y -> return(x,y)) y) x
    let map x ~f  = bind (fun y -> return(f y)) x
    let bind x ~f = bind f x
  end

end  
