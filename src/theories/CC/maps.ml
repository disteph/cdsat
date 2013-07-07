module type MapImplem = sig
  type e
  type v
  type t
  val find : e -> t -> v
  val empty: t
  val add: e -> v -> t -> t
  val union: t -> t -> t
  val remove: e -> t -> t
  val map : (v -> v) -> t -> t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
end

module type SetImplem = sig
  type e
  type t
  val empty : t
  val is_in : e -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val add : e -> t -> t
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
end
