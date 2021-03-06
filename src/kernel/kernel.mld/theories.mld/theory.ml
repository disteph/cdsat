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

  type ts
  val ts : ts Termstructures.Register.t

  type values
  val values : values values_opt

  type ('term,'value,'assign,'tset) api

  val make : (ts,values,'termdata,'value,'assign,'tset) dsProj
             -> ('termdata,'value,'assign,'tset) api
                    
end

module type Signature = sig
  type sign
  include Type
  type ('termdata,'value,'assign,'tset) signature
    = ('termdata*'value*'assign*'tset)
      *(sign*ts*values*('termdata,'value,'assign,'tset) api)
end
                     
module Make(T : sig
             type sign
             include Type
           end) = struct
  include T
  type ('termdata,'value,'assign,'tset) signature
    = ('termdata*'value*'assign*'tset)
      *(T.sign*T.ts*T.values*('termdata,'value,'assign,'tset) T.api)
end
