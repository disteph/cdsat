open Formulae
open Collection
open Strategy
open Sequents

(* Default implementation for interface FormulaImplem *)

module MyFormulaImplem = 
  (struct
     type t = Reveal of t form
     let reveal (Reveal a) = a
     let build a = (Reveal a)
   end : FormulaImplem)

(* Default implementation for interface CollectImplem *)

module type PrintableType = sig 
  type t 
  val toString: t -> string
end

module MyCollectImplem (MyPType:PrintableType) =
  (struct
     type e = MyPType.t
     type t = e list
     let is_empty = function 
	 [] -> true
       | _ -> false
     let rec is_in x = function
	 [] -> false
       | y::l when y=x -> true
       | y::l -> is_in x l
     let empty = [] 
     let add x l = x::l
     let rec union gamma1 = function
	 [] -> gamma1
       | a::gamma2 -> a::(union gamma1 gamma2)
     let rec inter gamma1 = function
	 [] -> []
       | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	   if is_in a gamma1 then a::gamma3 else gamma3
     let rec remove x = function
	 [] -> failwith(MyPType.toString(x)^" is not in list!")
       | y::l when y=x -> l
       | y::l -> y::(remove x l)
     let next = function
	 (a::l) -> (a,l)
       | _ -> failwith("No more item to pick")
     let rec toString = function
	 [] -> ""
       | f::[] -> MyPType.toString(f)
       | f::l -> MyPType.toString(f)^", "^(toString l)
     let hash = Hashtbl.hash
     let equal = (=)
   end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

(* Default implementation for interface ACollectImplem *)

module MyACollectImplem =
  (struct
     include MyCollectImplem(Atom)
     let rec filter b pred = function
       | []   -> []
       | a::l -> let l' = filter b pred l in
	   let (b',pred',tl) = Atom.reveal a in
	   if (b=b')&& (Atom.Predicates.compare pred pred' =0) then a::l' else l'
   end: ACollectImplem)

(* Default implementation for interface User *)

module MyUser:User = struct
  module UF = MyFormulaImplem
  module UFSet = MyCollectImplem(PrintableFormula(UF))
  module UASet = MyACollectImplem
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

      type data = unit
      let initial_data=()
      let rec solve = function
	| Local ans                       -> ans
	| Fake(Notify(_,machine,_))       -> solve (machine (None,(),fun _->Exit(Accept)))
	| Fake(AskFocus([],_,machine,_))  -> solve (machine (Restore))
	| Fake(AskFocus(a::l,_,machine,_))-> solve (machine (Focus(a, accept)))
	| Fake(AskSide(seq,machine,_))    -> solve (machine true)
	| Fake(Stop(b1,b2, machine))      -> solve (machine ())
	    
    end
end
