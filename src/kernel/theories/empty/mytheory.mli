open Top
open Messages
open Specs

type sign

module Make(DS: GTheoryDSType) : sig

  open DS

  type final = 
  | L of (sign,TSet.t,thProvable) thsays
  | R of (sign,TSet.t,thNotProvable) thsays

  val consistency: TSet.t -> final

end
