open Format

module Empty  = Theory.Make(Empty.MyTheory)
module Bool   = Theory.Make(Bool.MyTheory)
module CC     = Theory.Make(CC.MyTheory)
module Arrays = Theory.Make(Arrays.MyTheory)
module LRA    = Theory.Make(LRA.MyTheory)
module IfThenElse = Theory.Make(IfThenElse.MyTheory)
module FirstOrder = Theory.Make(FirstOrder.MyTheory)
       
module Tags = struct

  type _ t = 
    | Empty : (_,_,_) Empty.signature t
    | Bool  : (_,_,_) Bool.signature t
    | CC    : (_,_,_) CC.signature t
    | Arrays: (_,_,_) Arrays.signature t
    | LRA   : (_,_,_) LRA.signature t
    | IfThenElse: (_,_,_) IfThenElse.signature t
    | FirstOrder: (_,_,_) FirstOrder.signature t

  let id (type a) : a t -> int = function
    | Empty -> 1
    | Bool -> 2
    | CC    -> 3
    | Arrays -> 4
    | LRA -> 5
    | IfThenElse -> 6
    | FirstOrder -> 7
                
  let pp fmt (type a) : a t -> unit = function
    | Empty      -> fprintf fmt "Empty"
    | Bool       -> fprintf fmt "Bool"
    | CC         -> fprintf fmt "CC"
    | Arrays     -> fprintf fmt "Arrays"
    | LRA        -> fprintf fmt "LRA"
    | IfThenElse -> fprintf fmt "IfThenElse"
    | FirstOrder -> fprintf fmt "FirstOrder"
end

module Modules = struct

  let get (type tva)(type sign)(type ts)(type v)(type api)
        (tag : (tva*(sign*ts*v*api)) Tags.t)
       : ts Termstructures.Register.t * v Theory.values_opt
    = let open Tags in
      match tag with
    | Empty      -> Empty.ts, Empty.values
    | Bool       -> Bool.ts, Bool.values
    | CC         -> CC.ts, CC.values
    | Arrays     -> Arrays.ts, Arrays.values
    | LRA        -> LRA.ts, LRA.values
    | IfThenElse -> IfThenElse.ts, IfThenElse.values
    | FirstOrder -> FirstOrder.ts, FirstOrder.values
                             
  type _ t = Module : ('tva*(_*_*_*'api)) Tags.t * 'api -> 'tva t

  let make(type tva)(type sign)(type ts)(type v)(type api)
        (tag : (tva*(sign*ts*v*api)) Tags.t)
        (ds  : (ts,v,_,_,_) Top.Specs.dsProj)
    =
    let open Tags in
    match tag with 
    | Empty      -> Module(Empty,Empty.make ds)
    | Bool       -> Module(Bool,Bool.make ds)
    | CC         -> Module(CC,CC.make ds)
    | Arrays     -> Module(Arrays,Arrays.make ds)
    | LRA        -> Module(LRA,LRA.make ds)
    | IfThenElse -> Module(IfThenElse,IfThenElse.make ds)
    | FirstOrder -> Module(FirstOrder,FirstOrder.make ds)
                             
end

module Sig = struct

  type _ t = Sig : (_*('a*_*_*_)) Tags.t -> 'a t [@@unboxed]

  let id (Sig t) = Tags.id t
  let print_in_fmt fmt (Sig t) = Tags.pp fmt t

end

module Handlers = struct
  type t = Handler: (_*(_*_*_*_)) Tags.t -> t [@@unboxed]
  let id (Handler hdl) = Tags.id hdl
  let compare = id2compare id
  let print_in_fmt fmt (Handler hdl) = Tags.pp fmt hdl
end

let all_theories_list = 
  [
    Handlers.Handler Tags.Empty;
    Handlers.Handler Tags.Bool;
    Handlers.Handler Tags.CC;
    Handlers.Handler Tags.Arrays;
    Handlers.Handler Tags.LRA;
    Handlers.Handler Tags.IfThenElse;
  ]

module HandlersMap = struct
  include Map.Make(Handlers)

  let pp fmt hdls =
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

let all_theories = List.fold (fun hdl -> HandlersMap.add hdl ()) all_theories_list HandlersMap.empty

exception NotFound of string

let parse = function
  | "empty" | "prop" -> Handlers.Handler Tags.Empty
  | "CC"         -> Handlers.Handler Tags.CC
  | "LRA"        -> Handlers.Handler Tags.LRA
  | "LIA"        -> Handlers.Handler Tags.LRA
  | "Arrays"     -> Handlers.Handler Tags.Arrays
  | "IfThenElse" -> Handlers.Handler Tags.IfThenElse
  | "bool"       -> Handlers.Handler Tags.Bool
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
