open Formulae;;
open Collection;;
open Sequents;;

type ('a,'b) action = 
  | Focus of 'a*(('a,'b) receive)
  | Cut of int*'a*(('a,'b) receive)*(('a,'b) receive)
  | Polarise of string*bool*(('a,'b) receive)
  | Fake of bool
and ('a,'b) reception = 
  | Accept
  | Refuse
  | Action of ('a,'b) action
and ('a,'b) receive = ('b->('a,'b) reception)

let accept _ = Accept

module type UserStrategy =
  (sig
     module F: FormulaImplem
     module FSet: CollectImplem with type e = F.t
     module ASet: CollectImplem with type e = Atom.t

     val focus_pick : ASet.t*FSet.t*FSet.t*FSet.t*Sequents.polmap -> (F.t,Sequents.Answer(F)(FSet)(ASet).local) action
     val side_pick : ASet.t*F.t*FSet.t*FSet.t*Sequents.polmap  -> bool
   end
  )
;;

(* Default implementation for interface UserStrategy *)

module MyUserStrategy =
  (struct
     module F = MyFormulaImplem
     module FSet = MyCollectImplem(PrintableFormula(F))
     module ASet = MyCollectImplem(Atom)
     let focus_pick = function
	 (atomsN, a::l, formuPTried, formuPSaved,_) -> Focus(a, accept)
       | _ -> failwith("No more formula to place focus on.")
     let side_pick _ = true
   end:UserStrategy)
;;
