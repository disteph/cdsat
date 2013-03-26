open Kernel

open Formulae
open Interfaces

module GenPlugin(Atom: AtomType):(Plugins.Type with type literals = Atom.t) = struct

  type literals = Atom.t


  (* Default implementation for interface FormulaImplem *)

  module MyFormulaImplem = 
    (struct
       type lit = literals
       type t = Reveal of (t,lit) form
       let reveal (Reveal a) = a
       let build a = (Reveal a)
     end : FormulaImplem with type lit = literals)

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
	 | [] -> true
	 | _ -> false
       let rec is_in x = function
	 | [] -> false
	 | y::l when y=x -> true
	 | y::l -> is_in x l
       let empty = [] 
       let add x l = if is_in x l then l else x::l
       let rec union gamma1 = function
	 | [] -> gamma1
	 | a::gamma2 when is_in a gamma1 -> union gamma1 gamma2
	 | a::gamma2 -> a::(union gamma1 gamma2)
       let rec inter gamma1 = function
	 | [] -> []
	 | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	     if is_in a gamma1 then a::gamma3 else gamma3
       let rec remove x = function
	 | [] -> failwith(MyPType.toString(x)^" is not in list!")
	 | y::l when y=x -> l
	 | y::l -> y::(remove x l)
       let next = function
	 | (a::l) -> (a,l)
	 | _ -> failwith("No more item to pick")
       let rec fold f l0 init= match l0 with
	 | (a::l) -> fold f l (f a init)
	 | _ -> init
       let rec toString = function
	   [] -> ""
	 | f::[] -> MyPType.toString(f)
	 | f::l -> MyPType.toString(f)^", "^(toString l)
       let hash = Hashtbl.hash
       let equal = (=)
     end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

  (* Default implementation for interface ACollectImplem *)

  module MyACollectImplem = MyCollectImplem(Atom)
(* CollectImplem with type e = literals and type t = literals list) *)

  (* Default implementation for interface User *)

  module UF    = MyFormulaImplem
  module UFSet = MyCollectImplem(PrintableFormula(Atom)(UF))
  module UASet = MyACollectImplem
  module Strategy(FE:FrontEndType with type litType     = literals
				  and  type formulaType = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t)
    = struct
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
	| Local ans                              -> ans
	| Fake(Notify  (_,_,machine,_))          -> solve (machine (true,(),(fun _->Exit(Accept)),fun _->None))
	| Fake(AskFocus(_,[],true,_,machine,_))  -> solve (machine (Restore (fun _->None)))
	| Fake(AskFocus(_,[],false,_,machine,_)) -> solve (machine (ConsistencyCheck(accept,fun _->None)))
	| Fake(AskFocus(_,a::l,_,_,machine,_))   -> solve (machine (Focus(a, accept,fun _->None)))
	| Fake(AskSide (_,machine,_))            -> solve (machine true)
	| Fake(Stop(b1,b2, machine))             -> solve (machine ())
	    
    end

end
