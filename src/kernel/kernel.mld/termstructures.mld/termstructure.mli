open General

include module type of Termstructure_sig
    
type _ handler = private ..

module type Type = sig
  include PreType
  type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler
  val isHdl : ('data*'tset*'c) handler -> ('c,('data,'tset) t) Poly.iseq
end

module Make(PT : PreType) : Type with type ('data,'tset) t = ('data,'tset) PT.t
                                  and type ('data,'tset) api = ('data,'tset) PT.api
