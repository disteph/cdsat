open Format

type (_,_,_) th = private Th

module Tags = struct

  type _ t = 
    | Empty : (Empty.MyTheory.sign,
               unit,
               unit) th t
    | CC    : (CC.MyTheory.sign,
               unit,
               unit) th t
    | Arrays: (Arrays.MyTheory.sign,
               unit,
               unit) th t
    | Dejan : (Dejan.MyTheory.sign,
               unit,
               unit) th t
    | IfThenElse: (IfThenElse.MyTheory.sign,
                   unit,
                   unit) th t
    | Bool  : (Bool.MyTheory.sign,
               Bool.MyTheory.ts,
               unit) th t

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
                
module Sig = struct

  type _ t = Sig : ('a,_,_) th Tags.t -> 'a t [@@unboxed]

  let id (Sig t) = Tags.id t
  let print_in_fmt fmt (Sig t) = Tags.print_in_fmt fmt t

end

module Modules = struct
  open Top

  type ('term,'tset) t =
    (* | Empty : Empty.MyTheory.sign t *)
    (* | CC    : CC.MyTheory.sign t *)
    (* | Arrays: Arrays.MyTheory.sign t *)
    (* | Dejan : Dejan.MyTheory.sign t *)
    (* | IfThenElse: IfThenElse.MyTheory.sign t *)
    | Bool of ('term,'tset) Bool.MyTheory.t

  let get :
        (_,'ts,_) th Tags.t
        -> (module Theory.DSproj with type ts = 'ts
                                  and type Term.datatype = 'term
                                  and type TSet.t = 'tset)
        -> ((Variables.FreeVar.t,'term)Terms.term,'tset) t
    = fun x -> failwith "TODO"
                
end

               
module Handlers = struct
  type t = Handler: 'a Tags.t -> t [@@unboxed]
  let id (Handler hdl) = Tags.id hdl
  let compare = id2compare id
  let print_in_fmt fmt (Handler hdl) = Tags.print_in_fmt fmt hdl
end

let all_theories_list = 
  [ Handlers.Handler Tags.Empty;
    Handlers.Handler Tags.CC;
    Handlers.Handler Tags.Arrays;
    Handlers.Handler Tags.Dejan;
    Handlers.Handler Tags.IfThenElse;
    Handlers.Handler Tags.Bool;
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
  | "empty" | "prop" -> Handlers.Handler Tags.Empty
  | "CC"         -> Handlers.Handler Tags.CC
  | "LRA"        -> Handlers.Handler Tags.Dejan
  | "LIA"        -> Handlers.Handler Tags.Dejan
  | "Arrays"     -> Handlers.Handler Tags.Arrays
  | "IfThenElse" -> Handlers.Handler Tags.IfThenElse
  | "bool"       -> Handlers.Handler Tags.Bool
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
