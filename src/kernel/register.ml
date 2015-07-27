module Sig = struct

  type _ t = 
  | Empty: Empty.Mytheory.sign t

  let id (type a) (Empty: a t) = 0

end

type (_,_) thanswer = ThAns : 'a Sig.t * ('a,'tset,'b) Top.Messages.thsays -> ('tset,'b) thanswer

module Handlers = struct
  type t = Handler: 'a Sig.t -> t
  let id (Handler hdl) = Sig.id hdl
  let compare a b = Pervasives.compare (id a) (id b)
end

let all_theories_list = 
  [ Handlers.Handler Sig.Empty ]

module HandlersMap = Map.Make(Handlers)

let all_theories = List.fold_right (fun hdl -> HandlersMap.add hdl ()) all_theories_list HandlersMap.empty

exception NotFound of string

let parse = function
  | "prop" | "bool"  -> Handlers.Handler Sig.Empty
  | s -> raise (NotFound ("Theory "^s^" does not exist; see -help"))

let get_no l = List.fold_right (fun name -> HandlersMap.remove (parse name)) l all_theories
let get l = List.fold_right (fun name -> HandlersMap.add (parse name) ()) l HandlersMap.empty
