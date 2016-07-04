module Sig = struct

  type _ t = 
    | Empty : Empty.MyTheory.sign t
    | CC    : CC.MyTheory.sign t
    | Arrays: Arrays.MyTheory.sign t
    | Dejan : Dejan.MyTheory.sign t
    | IfThenElse: IfThenElse.MyTheory.sign t
    | Bool  : Bool.MyTheory.sign t

  let id (type a) : a t -> int = function
    | Empty -> 0
    | CC    -> 1
    | Arrays -> 2
    | Dejan -> 3
    | IfThenElse -> 4
    | Bool -> 5
    
  let print_in_fmt fmt (type a) : a t -> unit = function
    | Empty      -> Format.fprintf fmt "Empty"
    | CC         -> Format.fprintf fmt "CC"
    | Arrays     -> Format.fprintf fmt "Arrays"
    | Dejan      -> Format.fprintf fmt "Arithmetic"
    | IfThenElse -> Format.fprintf fmt "IfThenElse"
    | Bool       -> Format.fprintf fmt "Bool"
end

module Handlers = struct
  type t = Handler: 'a Sig.t -> t
  let id (Handler hdl) = Sig.id hdl
  let compare a b = Pervasives.compare (id a) (id b)
end

let all_theories_list = 
  [ Handlers.Handler Sig.Empty;
    Handlers.Handler Sig.CC;
    Handlers.Handler Sig.Arrays;
    Handlers.Handler Sig.Dejan;
    Handlers.Handler Sig.IfThenElse;
    Handlers.Handler Sig.Bool;
  ]

module HandlersMap = Map.Make(Handlers)

let all_theories = List.fold_right (fun hdl -> HandlersMap.add hdl ()) all_theories_list HandlersMap.empty

exception NotFound of string

let parse = function
  | "empty" | "prop" -> Handlers.Handler Sig.Empty
  | "CC"         -> Handlers.Handler Sig.CC
  | "LRA"        -> Handlers.Handler Sig.Dejan
  | "LIA"        -> Handlers.Handler Sig.Dejan
  | "Arrays"     -> Handlers.Handler Sig.Arrays
  | "IfThenElse" -> Handlers.Handler Sig.IfThenElse
  | "bool"       -> Handlers.Handler Sig.Bool
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold_right (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold_right (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
