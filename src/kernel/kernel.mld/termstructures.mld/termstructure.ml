open Top
open Interfaces_basic
open Specs

type _ handler = ..

module type PreType = sig

  type ('data,'tset) t

  module Make(Term : Term)(TSet : Collection with type e = Term.t): sig
    type nonrec t = (Term.datatype,TSet.t) t [@@deriving show]
    val build : proj:(Term.datatype -> t) -> Term.t -> t
  end
  
end
  
module type Type = sig
  include PreType
  type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler
  val isHdl
    : ('data*'tset*'c) handler -> ('data*'tset*'c,'data*'tset*('data,'tset) t) PolyEq.t
end

module Make(PT : PreType) : Type with type ('data,'tset) t = ('data,'tset) PT.t = struct
  include PT
  type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler
  let isHdl (type data tset c) : (data*tset*c) handler -> (data*tset*c,data*tset*(data,tset) t) PolyEq.t
    = function
    | Hdl -> PolyEq.Eq
    | _ -> PolyEq.NEq
end
