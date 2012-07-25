open Formulae;;
open Collection;;
open Sequents;;


module type User =
  (sig
     module UF: FormulaImplem
     module UFSet: CollectImplem with type e = UF.t
     module UASet: CollectImplem with type e = Atom.t

     module Strategy: 
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> 
       (sig
	  val solve : FE.Ans.output -> FE.Ans.t
	end)
   end
  )
;;

(* Default implementation for interface UserStrategy *)

module MyUser:User =
  (struct
     module UF = MyFormulaImplem
     module UFSet = MyCollectImplem(PrintableFormula(UF))
     module UASet = MyCollectImplem(Atom)
     module Strategy =
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) ->
	 (struct
	    include FE.Ans

	    let rec solve = function
	      | Local ans                   -> ans
	      | Fake(AskFocus(seq,machine)) ->
		  solve (machine (begin
			     match seq with
			       | FE.Seq.EntUF(_,_, a::l, _, _,_) -> Focus(a, accept)
			       | _ -> failwith("No more formula to place focus on.")
			   end))
	      | Fake(AskSide(seq,machine)) ->
		  solve(machine true)
	      | Fake(Stop(b1,b2, machine)) -> 
		  solve(machine ())

	  end)
   end)
;;
