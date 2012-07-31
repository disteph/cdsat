open Formulae;;
open Collection;;
open Strategy;;
open Sequents;;
open MyPatricia;;

include MyPatricia;;

module type PrintableOrderedType = sig 
  include PrintableType
  val compare: t->t->int
end;;

open Set

module MySmartCollectImplem =
  functor (MyPOType:PrintableOrderedType) -> struct
    module SS = Set.Make(MyPOType)
    type e       = SS.elt
    type t       = SS.t
    let is_empty = SS.is_empty
    let is_in    = SS.mem
    let empty    = SS.empty
    let add      = SS.add
    let union    = SS.union
    let inter    = SS.inter
    let remove   = SS.remove
    let choose   = SS.min_elt
    let hash     = Hashtbl.hash
    let equal    = SS.equal
    let next  t1 = let e1 = choose t1 in (e1, remove e1 t1)
    let toString t1 = 
      let rec toString_aux = function
	  [] -> ""
	| f::[] -> MyPOType.toString(f)
	| f::l -> MyPOType.toString(f)^", "^(toString_aux l)
      in
	toString_aux (SS.elements t1)
  end
;;

module MySmartUser =
  (struct

     (* User uses the smart datastructures with hconsing and sets from
     above *)

     module UF    = MyOrderedSmartFormulaImplem
     module UFSet = MySmartCollectImplem(struct 
				include PrintableFormula(UF)
				let compare a b = UF.compare a b
			      end)
     module UASet = MyCollectImplem(Atom)

     (* Below are the restricted version of the above, where the
     peculiarities of the implementations are hidden before these are
     fed to the frontend *)
     
     module F: (FormulaImplem with type t = UF.t) = UF
     module FSet: (CollectImplem with type e = F.t and type t=UFSet.t) = UFSet
     module ASet: (CollectImplem with type e = Atom.t and type t=UASet.t) = UASet

     module Strategy =
       functor (FE:FrontEndType with module F=F and module FSet=FSet and module ASet=ASet) -> struct
	 include FE

	 (* As in the default implementation of user's strategies,
	    this strategy provides the following function solve:
	    In case the temporary answers happens to be a final
	    answer, then the strategy returns that final answer.
	    Otherwise, the temporary answer always contains a
	    computing machine that can be triggered by inserting a
	    "coin" - the user can orient the computation by
	    choosing which coin they insert (typically, which
	    formula to place in the next focus - here: the first
	    available one, but using the choose function of
	    UFSet) *)

	 let rec solve = function
	   | Local ans                  -> ans
	   | Fake(AskFocus(seq,machine))-> solve (machine (match seq with
							     | Seq.EntUF(_,_, l, _, _,_) -> Focus(UFSet.choose l, accept)
							     | _ -> failwith("No more formula to place focus on.")
							  ))
	   | Fake(AskSide(seq,machine)) -> solve(machine true)
	   | Fake(Stop(b1,b2, machine)) -> solve(machine ())

       end
   end:User)
;;
