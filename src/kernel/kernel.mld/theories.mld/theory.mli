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

  (* ts is the type of term representations used by the theory module *)
  type ts
  (* ts is the handler for this term representation module *)
  val ts : ts Termstructures.Register.t

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
  type ('term,'value,'assign) api

  (* How to build an API *)
  val make : (ts,values,'termdata,'value,'assign) dsProj
             -> ('termdata,'value,'assign) api
                    
end

module type Signature = sig
  type sign
  include Type
  type ('termdata,'value,'assign) signature
    = ('termdata*'value*'assign)
      *(sign*ts*values*('termdata,'value,'assign) api)
end

module Make(T : sig
             (* sign is the secret type used by the theory module to label its messages *)
             type sign
             include Type
           end) : Signature with type sign = T.sign
                             and type ts = T.ts
                             and type values = T.values
                             and type ('t,'v,'a) api = ('t,'v,'a) T.api
