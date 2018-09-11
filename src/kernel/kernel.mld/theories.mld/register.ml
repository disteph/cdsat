open Format

open General
open Top.Terms

open Theory
    
module Modules = struct

  type t = Module : (_*'api) Tags.t * 'api -> t

  let make w = function
    | Handlers.Handler hdl -> Module(hdl,Tags.make hdl w)
    | Handlers.Eq -> failwith "TO DO"
                             
end

module HandlersMap = struct
  include Map.Make(Handlers)

  let pp_binding ?pp_v fmt (hdl,v) =
    match pp_v with
    | Some f -> Format.fprintf fmt "(%a↦%a)" Handlers.pp hdl f v
    | None -> Format.fprintf fmt "%a" Handlers.pp hdl

  let pp_opt ?pp_v fmt hdlmap =
    List.pp (pp_binding ?pp_v) fmt (bindings hdlmap)

  let pp fmt hdls = pp_opt fmt hdls
                           
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

let all_theories = List.fold (fun hdl -> HandlersMap.add hdl ()) !all_theories_list HandlersMap.empty

exception NotFound of string

let parse = function
  | "LRA"        -> Handlers.Handler LRA.MyTheory.hdl
  | "Arrays"     -> Handlers.Handler Arrays.MyTheory.hdl
  | "IfThenElse" -> Handlers.Handler IfThenElse.MyTheory.hdl
  | "bool"       -> Handlers.Handler Bool.MyTheory.hdl
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
