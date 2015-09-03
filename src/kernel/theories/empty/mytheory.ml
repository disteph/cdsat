open Top
open Messages
open Specs

type sign = unit

module Make(DS: GTheoryDSType) = struct

  open DS

  type final = 
  | L of (sign,TSet.t,thProvable) thsays
  | R of (sign,TSet.t,thNotProvable) thsays

  let goal_consistency t atomN =
    if TSet.mem t atomN
    then L(thProvable () (TSet.add t TSet.empty))
    else R(thNotProvable () atomN)

  let consistency atomN = TSet.fold
    (fun l -> function
    | L(ThProvable set) as ans -> ans
    | _ ->
      begin
        match goal_consistency (Term.bC Symbols.Neg [l]) atomN with
        | L(ThProvable set) -> L(thProvable () (TSet.add l set))
        | ans -> ans 
      end
    )
    atomN
    (R(thNotProvable () atomN))

end
