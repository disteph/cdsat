open Top.Variables

type t = MakesSense.t

let bV _ fv = MakesSense.fv fv

let bC _ _ l =
  List.fold MakesSense.combine l MakesSense.init
