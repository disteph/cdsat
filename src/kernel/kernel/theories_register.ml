open Format
       
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
    | Empty      -> fprintf fmt "Empty"
    | CC         -> fprintf fmt "CC"
    | Arrays     -> fprintf fmt "Arrays"
    | Dejan      -> fprintf fmt "Arithmetic"
    | IfThenElse -> fprintf fmt "IfThenElse"
    | Bool       -> fprintf fmt "Bool"
end

module Handlers = struct
  type t = Handler: 'a Sig.t -> t [@@unboxed]
  let id (Handler hdl) = Sig.id hdl
  let compare = id2compare id
  let print_in_fmt fmt (Handler hdl) = Sig.print_in_fmt fmt hdl
end

let all_theories_list = 
  [ Handlers.Handler Sig.Empty;
    Handlers.Handler Sig.CC;
    Handlers.Handler Sig.Arrays;
    Handlers.Handler Sig.Dejan;
    Handlers.Handler Sig.IfThenElse;
    Handlers.Handler Sig.Bool;
  ]

module HandlersMap = struct
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

let all_theories = List.fold (fun hdl -> HandlersMap.add hdl ()) all_theories_list HandlersMap.empty

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

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
