exception IncoherentTable

module type Keys = sig
  type 'a t
  val hash  : 'a t -> int
  val equal : 'a t -> 'b t -> ('a,'b) Poly.iseq
end

module type S = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val mem : 'b t -> 'a key -> bool
  val find     : 'b t -> 'a key -> ('a,'b) data
  val add      : 'b t -> 'a key -> ('a,'b) data -> unit
  val remove   : 'b t -> 'a key -> unit

  val create  : int -> 'b t
  val clear   : 'b t -> unit

  type ('b,'c) fold = { fold: 'a. 'a key -> ('a,'b) data -> 'c -> 'c }
  val fold : ('b,'c) fold -> 'b t -> 'c -> 'c

  val copy : 'b t -> 'b t

  type printk    = { printk: 'a. 'a key Format.printer }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Format.printer }
  val pp: string -> string -> printk -> 'b printd -> 'b t Format.printer

end

(** Same as S but for ('a,'b) data = 'b *)
module type R = sig
  include S with type ('a,'b) data = 'b
  (* Some primitives get their types simplified *)
  val fold  : ('b -> 'c -> 'c) -> 'b t -> 'c -> 'c
  val pp: string -> string -> printk -> 'b Format.printer -> 'b t Format.printer
end

(** Same as S but for ('a,'b) data = 'a *)
module type T = sig
  type t
  type 'a key
  val mem  : t -> 'a key -> bool
  val find : t -> 'a key -> 'a
  val add  : t -> 'a key -> 'a -> unit
  val remove   : t -> 'a key -> unit
  val create : int -> t
  val clear : t -> unit
  type 'c fold = { fold : 'a. 'a key -> 'a -> 'c -> 'c }
  val fold : 'c fold -> t -> 'c -> 'c
  val copy : t -> t
  type printk = { printk : 'a. 'a key Format.printer; }
  type printd = { printd : 'a. 'a key -> 'a Format.printer; }
  val pp : string -> string -> printk -> printd -> t Format.printer
end
