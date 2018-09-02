open General

include Termstructure_sig

type _ handler = ..

module type Type = sig
  include PreType
  type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler
  val isHdl : ('data*'tset*'c) handler -> ('c,('data,'tset) t) Poly.iseq
end

module Make(PT : PreType) = struct
  include PT
  type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler
  let isHdl (type data tset c) : (data*tset*c) handler -> (c,(data,tset) t) Poly.iseq
    = function
    | Hdl -> Poly.Eq
    | _ -> Poly.Neq

  (* module Cmp(M : sig
   *     type ('d,'tset) t
   *     type _ handler += Hdl : ('data*'tset*('data,'tset) t) handler    
   *   end) = struct
   * 
   *   exception No
   * 
   *   let iseq (type d ts) (): ((d,ts) M.t,(d,ts) t) Poly.eq =
   *     Poly.eq (isHdl (M.Hdl : (d*ts*(d,ts) M.t) handler))
   * 
   *   type iseq = Eq of {eq: 'd 'ts. unit -> (('d,'ts) M.t,('d,'ts) t) Poly.eq} | Neq
   *   let iseq2 =
   *     try 
   *       Eq{eq = iseq }
   *     with
   *     | No -> Neq
   * 
   * end *)
end
