open Top.Specs

open Prop
open Literals

type sign = unit

module Make(DS: sig 
  include GTheoryDSType
  val proj: Term.datatype -> LitF.t
end) = struct

  module Termproj = struct
    include DS.Term
    let proj = DS.proj 
  end
    
  include CCX.Make
    (DS)
    (EmptyCC.Make(Termproj))
    (EmptyU.Make(Termproj))

end
