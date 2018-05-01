(**************************************************************)
(* This is the specification of of theory module, kernel part *)
(**************************************************************)

open Top
open Specs
open Interfaces_basic

type _ values_opt = private
  | HasValues : (module PH with type t = 'a) -> 'a has_values values_opt
  | HasNoValues : has_no_values values_opt

module HasValues(V:PH) : sig
  type values = V.t has_values
  val values : values values_opt
end

module HasNoValues : sig
  type values = has_no_values
  val values : values values_opt
end

module type Type = sig

  (* TS is the term representations used by the theory module *)
  module TS : Termstructures.Termstructure.Type

  (* type values is
     - either has_no_values if the theory module does not use values
     (other than the Boolean ones)
     - or, if it does, of the form v has_values, where v is the type of its values.

     val values provides the functions manipulating values,
     should the theory module use values

     Neither the type nor the val should be written by hand directly. Use
       include HasValues(struct type t = ... let compare = ... end)
     if you want to use values, or
       include HasNoValues
     if you don't.
 *)
              
  type values
  val values : values values_opt
                    
  (* This type defines the API that the theory module offers to the plugins that pilot it.
     The API is specific to each theory, 
     and is parameterised by the global types for term data, values, and assignments.
     Typically, it will be of the form
       type ('term,'value,'assign) api = (module API with type term = 'term
                                                           and type value = 'value
                                                           and type assign = 'assign)
   *)
  type ('term,'value,'assign,'tset) api

  (* How to build an API *)
  val make : (('termdata,'tset) TS.t,values,'termdata,'value,'assign,'tset) dsProj
             -> ('termdata,'value,'assign,'tset) api
                    
end

module type WithSign = sig
  include Type
  (* sign is the secret type used by the theory module to label its messages *)
  type sign
end

type _ handler = private ..

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

module Make(T : WithSign) : WithHandler with module T = T
