open Formulae;;
open Collection;;
open Sequents;;


module type User =
  (sig

     (* A user should provide an implementation of formulae, an
	implementation of sets of formulae, and an implementation of
	sets of atoms *)

     module UF: FormulaImplem
     module UFSet: CollectImplem with type e = UF.t
     module UASet: CollectImplem with type e = Atom.t

     (* A user should provide a strategy: given the datastructures of
	a FrontEnd (implementation of sequents, answers, outputs,
	etc), the strategy provides a function solve a function that
	should convert a temporary answer (output) into a final answer
	(t).  See the default implementation below *)

     module Strategy: 
       functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> 
       (sig
	  val solve : FE.output -> FE.t
	end)
   end
  )
;;

(* Default implementation for interface User *)

module MyUser:User = struct
  module UF = MyFormulaImplem
  module UFSet = MyCollectImplem(PrintableFormula(UF))
  module UASet = MyCollectImplem(Atom)
  module Strategy =
    functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> struct
      include FE
	(* The strategy provides the following function solve:
	   In case the temporary answers happens to be a final
	   answer, then the strategy returns that final answer.
	   Otherwise, the temporary answer always contains a
	   computing machine that can be triggered by inserting a
	   "coin" - the user can orient the computation by
	   choosing which coin they insert (typically, which
	   formula to place in the next focus - here: the first
	   available one) *)

      let rec solve = function
	| Local ans                   -> ans
	| Fake(AskFocus(seq,machine)) ->
	    solve (machine (match seq with
			      | Seq.EntUF(_,_, a::l, _, _,_) -> Focus(a, accept)
			      | _ -> failwith("No more formula to place focus on.")
			   ))
	| Fake(AskSide(seq,machine)) -> solve(machine true)
	| Fake(Stop(b1,b2, machine)) -> solve(machine ())
    end
end;;
