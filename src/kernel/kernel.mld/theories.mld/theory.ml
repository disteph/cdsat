open Top
open Specs
open Interfaces_basic

type _ values_opt =
  | HasValues : (module PH with type t = 'a) -> 'a has_values values_opt
  | HasNoValues : has_no_values values_opt

module HasValues(V:PH) = struct
  type values = V.t has_values
  let values = HasValues(module V)
end

module HasNoValues = struct
  type values = has_no_values
  let values = HasNoValues
end

module type Type = sig

  module TS : Termstructures.Termstructure.Type

  type values
  val values : values values_opt

  type ('term,'value,'assign,'tset) api

  val make : (('termdata,'tset) TS.t,values,'termdata,'value,'assign,'tset) dsProj
             -> ('termdata,'value,'assign,'tset) api
                    
end

module type WithSign = sig
  include Type
  (* sign is the secret type used by the theory module to label its messages *)
  type sign
end

type _ handler = ..

module type WithHandler = sig

  module T : WithSign
  open T

  type ('data,'value,'assign,'tset) signature
    = ('data*'value*'assign*'tset)
      *(sign*('data,'tset) TS.t*values*('data,'value,'assign,'tset) api)

  type _ handler += Hdl : ('data,'value,'assign,'tset) signature handler

  val isHdl : 
      (('data*'value*'assign*'tset)*('sign*'ts*'values*'api)) handler
      -> (('data,'value,'assign,'tset) signature,
          ('data*'value*'assign*'tset)*('sign*'ts*'values*'api)) PolyEq.t
end


module Make(T : WithSign) = struct
  module T = T
  open T
  type ('data,'value,'assign,'tset) signature
    = ('data*'value*'assign*'tset)
      *(sign*('data,'tset) TS.t*values*('data,'value,'assign,'tset) api)
  type _ handler += Hdl : ('data,'value,'assign,'tset) signature handler
  let isHdl
    : type data value assign tset sign ts values api.
      ((data*value*assign*tset)*(sign*ts*values*api)) handler
      -> ((data,value,assign,tset) signature,
          (data*value*assign*tset)*(sign*ts*values*api)) PolyEq.t
    = function
      | Hdl -> PolyEq.Eq
      | _ -> PolyEq.NEq
end
