open Format

module Bool   = Theory.Make(Bool.MyTheory)
module Arrays = Theory.Make(Arrays.MyTheory)
module LRA    = Theory.Make(LRA.MyTheory)
module IfThenElse = Theory.Make(IfThenElse.MyTheory)
       
module Tags = struct

  type 'a t = 'a Theory.handler

  let[@inline] id (type a) : a t -> int = function
    | Bool.Hdl -> 2
    | Arrays.Hdl -> 3
    | LRA.Hdl -> 4
    | IfThenElse.Hdl -> 5
    | _ -> failwith "You forgot to map a theory to an int"
                
  let pp fmt (type a) : a t -> unit = function
    | Bool.Hdl       -> fprintf fmt "Bool"
    | Arrays.Hdl     -> fprintf fmt "Arrays"
    | LRA.Hdl        -> fprintf fmt "LRA"
    | IfThenElse.Hdl -> fprintf fmt "IfThenElse"
    | _ -> failwith "You forgot to give a name to a theory"

  let eq  (type data value assign tset sign1 ts1 values1 api1 sign2 ts2 values2 api2)
    : ((data*value*assign*tset)*(sign1*ts1*values1*api1)) Theory.handler
      -> ((data*value*assign*tset)*(sign2*ts2*values2*api2)) Theory.handler
      -> ((data*value*assign*tset)*(sign1*ts1*values1*api1),
          (data*value*assign*tset)*(sign2*ts2*values2*api2)) PolyEq.t
    = function
    | Bool.Hdl       -> Bool.isHdl
    | Arrays.Hdl     -> Arrays.isHdl
    | LRA.Hdl        -> LRA.isHdl
    | IfThenElse.Hdl -> IfThenElse.isHdl
    | _ -> failwith "You forgot to map a theory to its eq function"
    
end

module Modules = struct

  let get (type gv a termdata tset sign ts v api)
        (tag : ((termdata*gv*a*tset)*(sign*ts*v*api)) Tags.t)
       : (module Theory.Type)
    = match tag with
    | Bool.Hdl       -> (module Bool.T)
    | Arrays.Hdl     -> (module Arrays.T)
    | LRA.Hdl        -> (module LRA.T)
    | IfThenElse.Hdl -> (module IfThenElse.T)
    | _ -> failwith "Forgot to map a theory to its module"
                             
  type _ t = Module : ('tva*(_*_*_*'api)) Tags.t * 'api -> 'tva t

  let make(type data gv assign tset sign ts v api)
        (tag : ((data*gv*assign*tset)*(sign*ts*v*api)) Tags.t)
        (ds  : (ts,v,data,_,_,tset) Top.Specs.dsProj)
    =
    match tag with 
    | Bool.Hdl       -> Bool.(Module(Hdl,T.make ds))
    | Arrays.Hdl     -> Arrays.(Module(Hdl,T.make ds))
    | LRA.Hdl        -> LRA.(Module(Hdl,T.make ds))
    | IfThenElse.Hdl -> IfThenElse.(Module(Hdl,T.make ds))
    | _ -> failwith "Forgot to map a theory to its make function"
                             
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
    Handlers.Handler Bool.Hdl;
    Handlers.Handler Arrays.Hdl;
    Handlers.Handler LRA.Hdl;
    Handlers.Handler IfThenElse.Hdl;
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
  | "LRA"        -> Handlers.Handler LRA.Hdl
  | "Arrays"     -> Handlers.Handler Arrays.Hdl
  | "IfThenElse" -> Handlers.Handler IfThenElse.Hdl
  | "bool"       -> Handlers.Handler Bool.Hdl
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty

              
