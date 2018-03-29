type 'x t = unit -> 'x node_t
and 'x node_t = Nil | Cons of ('x * 'x t)
val nil : unit -> 'a node_t
val cons : 'a -> 'a t -> unit -> 'a node_t
val next : (unit -> 'a) -> 'a
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val peek : (unit -> 'a node_t) -> 'a option
val get : (unit -> 'a node_t) -> ('a * 'a t) option
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val append : 'a t -> 'a t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val exists : ('a -> bool) -> 'a t -> bool
val lazy_fold_right :
  ('a -> (unit -> 'b) -> 'b) -> 'a t -> (unit -> 'b) -> unit -> 'b
val concat : 'a t t -> 'a t
val concat_map : ('a -> 'b t) -> 'a t -> 'b t
val length : 'a t -> int
val from_loop : ('a -> ('b * 'a) option) -> 'a -> 'b t
val last : 'a t -> 'a option
val take : int -> 'a t -> 'a t
val range : int -> int -> int t
