open Theory
    
module Modules : sig
  type t = private Module : (_*'api) Tags.t * 'api -> t
  val make : (module Top.Terms.Writable) -> (_*_) Tags.t -> t
end

module HandlersMap : sig
  include Map.S with type key = Handlers.t
  val pp_binding : ?pp_v:('a Format.printer) -> (Theory.Handlers.t * 'a) Format.printer
  val pp_opt    : ?pp_v:('a Format.printer) -> 'a t Format.printer
  val pp        : 'a t Format.printer
  val union_aux : 'a -> 'b option -> 'b option -> 'b option
  val union     : 'a t -> 'a t -> 'a t
  val inter_aux : 'a -> 'b option -> 'c option -> 'c option
  val inter     : 'a t -> 'b t -> 'b t
  val diff_aux  : 'a -> 'b option -> 'c option -> 'b option
  val diff      : 'a t -> 'b t -> 'a t
end

val all_theories : unit -> unit HandlersMap.t

exception NotFound of string

val parse  : string -> Handlers.t
val get_no : string list -> unit HandlersMap.t
val get    : string list -> unit HandlersMap.t
