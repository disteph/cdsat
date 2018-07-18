open Top
open Basic
open Specs

type _ handler = private ..

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

module Make(PT : PreType) : Type with type ('data,'tset) t = ('data,'tset) PT.t
