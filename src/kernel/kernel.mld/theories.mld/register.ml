open Format

module Bool   = Theory.Make(Bool.MyTheory)
module Arrays = Theory.Make(Arrays.MyTheory)
module LRA    = Theory.Make(LRA.MyTheory)
module IfThenElse = Theory.Make(IfThenElse.MyTheory)
       
module Tags = struct

  type _ t = 
    | Bool  : (_,_,_,_) Bool.signature t
    | Arrays: (_,_,_,_) Arrays.signature t
    | LRA   : (_,_,_,_) LRA.signature t
    | IfThenElse: (_,_,_,_) IfThenElse.signature t

  let[@inline] id (type a) : a t -> int = function
    | Bool -> 2
    | Arrays -> 3
    | LRA -> 4
    | IfThenElse -> 5
                
  let pp fmt (type a) : a t -> unit = function
    | Bool       -> fprintf fmt "Bool"
    | Arrays     -> fprintf fmt "Arrays"
    | LRA        -> fprintf fmt "LRA"
    | IfThenElse -> fprintf fmt "IfThenElse"

  module TestEq(M : sig
               type (_,_,_,_,_,_) t
             end) = struct
    open General.Sums
    let eq
          (type s1 t1 v1 tva1 api1
                  s2 t2 v2 tva2 api2)
          (tag1 : (tva1*(s1*t1*v1*api1)) t)
          (tag2 : (tva2*(s2*t2*v2*api2)) t)
          (iftrue  : (s1,t1,v1,s1,t1,v1) M.t)
          (iffalse : (s1,t1,v1,s2,t2,v2) M.t)
        : (s1,t1,v1,s2,t2,v2) M.t
      =
      match tag1,tag2 with
      | Bool, Bool   -> iftrue
      | Arrays, Arrays -> iftrue
      | LRA, LRA       -> iftrue
      | IfThenElse, IfThenElse -> iftrue
      | _ -> iffalse
  end

  module M = TestEq(struct
                       type (_,_,_,_,_,_) t = bool
                     end)

  let eq tag1 tag2 = M.eq tag1 tag2 true false

end

module Modules = struct

  let get (type tva sign ts v api)
        (tag : (tva*(sign*ts*v*api)) Tags.t)
       : ts Termstructures.Register.t * v Theory.values_opt
    = let open Tags in
      match tag with
    | Bool       -> Bool.ts, Bool.values
    | Arrays     -> Arrays.ts, Arrays.values
    | LRA        -> LRA.ts, LRA.values
    | IfThenElse -> IfThenElse.ts, IfThenElse.values
                             
  type _ t = Module : ('tva*(_*_*_*'api)) Tags.t * 'api -> 'tva t

  let make(type tva sign ts v api)
        (tag : (tva*(sign*ts*v*api)) Tags.t)
        (ds  : (ts,v,_,_,_,_) Top.Specs.dsProj)
    =
    let open Tags in
    match tag with 
    | Bool       -> Module(Bool,Bool.make ds)
    | Arrays     -> Module(Arrays,Arrays.make ds)
    | LRA        -> Module(LRA,LRA.make ds)
    | IfThenElse -> Module(IfThenElse,IfThenElse.make ds)
                             
end

module Handlers = struct
  type t =
    | Handler: (_*(_*_*_*_)) Tags.t -> t
    | Eq
  let id = function
    | Handler hdl -> Tags.id hdl +1
    | Eq -> 0
                             
  let compare = Compare.id2compare id
  let pp fmt = function
    | Handler hdl -> Tags.pp fmt hdl
    | Eq -> Format.fprintf fmt "Eq"
end

let all_theories_list = 
  [
    Handlers.Handler Tags.Bool;
    Handlers.Handler Tags.Arrays;
    Handlers.Handler Tags.LRA;
    Handlers.Handler Tags.IfThenElse;
    Handlers.Eq
  ]

    
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

let all_theories = List.fold (fun hdl -> HandlersMap.add hdl ()) all_theories_list HandlersMap.empty

exception NotFound of string

let parse = function
  | "LRA"        -> Handlers.Handler Tags.LRA
  | "Arrays"     -> Handlers.Handler Tags.Arrays
  | "IfThenElse" -> Handlers.Handler Tags.IfThenElse
  | "bool"       -> Handlers.Handler Tags.Bool
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty

              
