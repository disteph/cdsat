module FVSubst = struct
  open Variables
  type t = FreeVar.t DSubst.t [@@deriving eq,show,hash]
  let get_arity = DSubst.get_arity
  let get = DSubst.get FreeVar.pp       
end
