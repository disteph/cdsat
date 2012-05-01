open Formulae;;
open Collection;;
open Sequents;;

module type UserStrategy =
  (sig
     module F: FormulaImplem
     module FSet: CollectImplem with type e = F.t
     module ASet: CollectImplem with type e = Atom.t
     val focus_pick : ASet.t*FSet.t*FSet.t*FSet.t -> F.t
     val side_pick : ASet.t*F.t*FSet.t*FSet.t*FSet.t -> bool
   end
  )
;;

module MyUserStrategy =
  (struct
     module F = MyFormulaImplem
     module FSet = MyCollectImplem(Formula(F))
     module ASet = MyCollectImplem(Atom)
     let focus_pick(atomsN, a::l, formuPTried, formuPSaved) = a
     let side_pick(atomsN, focused, formuP, formuPTried, formuPSaved) = true
   end:UserStrategy)
;;
