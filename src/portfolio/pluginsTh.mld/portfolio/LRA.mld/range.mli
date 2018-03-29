type _ t [@@deriving show]
          
val init : 'a t
val pick : 'a t -> Q.t
val mem : Q.t -> 'a t -> bool

type 'a update =
  | Range of 'a t
  | FourierMotzkin of 'a*'a
  | DisEqual of 'a*'a*'a

val upper_update : Q.t -> is_strict:bool -> 'a -> 'a t -> 'a update
val lower_update : Q.t -> is_strict:bool -> 'a -> 'a t -> 'a update
val diseq_update : Q.t -> 'a -> 'a t -> 'a update
