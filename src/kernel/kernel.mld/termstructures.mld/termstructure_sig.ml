open Top
open Basic
open Specs

module type PreType = sig

  type ('data,'tset) t
  type ('data,'tset) api

  module Make(Term : Term)(TSet : Collection with type e = Term.t): sig
    type nonrec t = (Term.datatype,TSet.t) t [@@deriving show]
    val build : proj:(Term.datatype -> t) -> Term.t -> t
    val api : (Term.datatype,TSet.t) api
  end
  
end
