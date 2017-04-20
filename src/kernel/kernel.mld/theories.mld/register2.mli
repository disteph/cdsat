open Format

module Empty  = Theory.Make(Empty.MyTheory)
module CC     = Theory.Make(CC.MyTheory)
module Arrays = Theory.Make(Arrays.MyTheory)
module LRA    = Theory.Make(LRA.MyTheory)
module IfThenElse = Theory.Make(IfThenElse.MyTheory)
module Bool   = Theory.Make(Bool.MyTheory)
       
module Tags : sig

  type _ t = 
    | Empty : (_,_,_) Empty.signature t
    | CC    : (_,_,_) CC.signature t
    | Arrays: (_,_,_) Arrays.signature t
    | LRA   : (_,_,_) LRA.signature t
    | IfThenElse: (_,_,_) IfThenElse.signature t
    | Bool  : (_,_,_) Bool.signature t
                [@@deriving show]

  val id : 'a t -> int

end

module Modules : sig

  val get : ('tva*('sign*'ts*'v*'api)) Tags.t
            -> 'ts Termstructures.Register.t
               * 'v Theory.values_opt
                             
  type _ t = Module : ('tva*(_*_*_*'api)) Tags.t * 'api -> 'tva t

  val make : (('t*'v*'a)*(_*'ts*'v*_)) Tags.t
             -> ('ts,'v,_,_,_) Top.Specs.dsProj
             -> ('t*'v*'a) t     
end

module Sig : sig

  type _ t = Sig : ('a*_*_) Tags.t -> 'a t [@@unboxed]

  val id : _ t -> int
  val pp : Format.formatter -> _ t -> unit

end

module Handlers : sig
  type t = Handler: 'a Tags.t -> t [@@unboxed] [@@deriving ord, pp]
  val id : t -> int
end

val all_theories_list = Handlers.t list

module HandlersMap : sig
  include Map.Make(Handlers)

  let print_in_fmt fmt hdls =
    let _ =
      fold
        (fun a _ b ->
          fprintf fmt "%s%a" (if b then ", " else "") Handlers.print_in_fmt a; true)
        hdls false in
    ()

  let union_aux _ a b = match a,b with
    | None, None -> None
    | Some v, None | None, Some v | Some _, Some v -> Some v

  let union a b = merge union_aux a b

  let inter_aux _ a b = match a,b with
    | Some _, Some v -> Some v
    | Some _, None | None, Some _ | None, None -> None
      
  let inter a b = merge inter_aux a b

  let diff_aux _ a b = match b with
    | Some v -> None
    | None -> a
                                                    
  let diff a b = merge diff_aux a b

end

val all_theories : 

exception NotFound of string

val parse : string -> Handlers.t

val get_no : string list ->
val get : string list ->
