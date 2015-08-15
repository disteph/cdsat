open Top
open Specs
open Messages

open Prop
open Literals

type sign = unit

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> LitF.t
end) = struct

  open DS

  type final = 
  | L of (sign,TSet.t,thProvable) thsays
  | R of (sign,TSet.t,thNotProvable) thsays

  module Termproj = struct
    include DS.Term
    let proj = DS.proj 
  end
    
  module CCXMade = CCX.Make(DS)(EmptyCC.Make(Termproj))(EmptyU.Make(Termproj))

  let consistency atomN = 
    match CCXMade.consistency atomN with
    | None     -> R(thNotProvable () atomN)
    | Some set -> L(thProvable () set)

end
