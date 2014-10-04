open Format

open Kernel

open Interfaces_I
open Formulae
open Interfaces_II

module GenPlugin(IAtom: IAtomType)
  :(Plugins.Type with type iliterals = IAtom.t
                 and  type literals  = IAtom.Atom.t
                 and  type delsubsts = IAtom.DSubst.t) = struct
  
  type iliterals = IAtom.t
  type literals  = IAtom.Atom.t
  type delsubsts = IAtom.DSubst.t
    
  (* Default implementation for interface CollectImplem *)

  module type PrintableType = sig 
    type t 
    val print_in_fmt: formatter -> t -> unit
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
	 | [] -> failwith(Dump.toString (fun p->p "%a is not in list!" MyPType.print_in_fmt x))
	 | y::l when y=x -> l
	 | y::l -> y::(remove x l)
       let next = function
	 | (a::l) -> (a,l)
	 | [] -> failwith("No more item to pick")
       let rec fold f l0 init= match l0 with
	 | (a::l) -> fold f l (f a init)
	 | [] -> init
       let subset gamma1 gamma2 =
         fold (fun a b ->b && is_in a gamma2) gamma1 true

       let rec print_in_fmt fmt = function
	 | []    -> ()
	 | f::[] -> fprintf fmt "%a" MyPType.print_in_fmt f
	 | f::l  -> fprintf fmt "%a, %a" MyPType.print_in_fmt f print_in_fmt l

     end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

  module UASet = MyCollectImplem(IAtom)
  module UF = struct
    type lit = literals
    type t = unit
    let fdata_build f = ()
  end
  module UFSet = MyCollectImplem(struct
    type t = (UF.t,UF.lit) GForm.t * IAtom.DSubst.t
    let print_in_fmt = GForm.iprint_in_fmt IAtom.Atom.print_in_fmt IAtom.DSubst.print_in_fmt
  end)

  module Strategy(FE:FrontEndType with type Form.lit    = literals
				  and  type Form.datatype = UF.t
				  and  type fsetType    = UFSet.t
				  and  type asetType    = UASet.t
				  and  type ilit        = iliterals
				  and  type dsubsts     = delsubsts) = struct
      include FE
      include Common.Utils.FEext(FE)
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
      let initial_data _ _=()
      let rec solve = function
	| Jackpot ans                                -> ans
	| InsertCoin(Notify(_,_,_,machine,_))            -> solve (machine (true,(fun _->()),accept,fNone))
	| InsertCoin(AskFocus(_,_,[],true,_,machine,_))  -> solve (machine (Restore((fun _->()),accept,fNone)))
	| InsertCoin(AskFocus(_,_,[],false,_,machine,_)) -> solve (machine (ConsistencyCheck((fun _->()),accept,fNone)))
	| InsertCoin(AskFocus(_,_,a::l,_,_,machine,_))   -> solve (machine (Focus(a,((fun _->()),(fun _->())),accept,fNone)))
	| InsertCoin(AskSide (_,_,machine,_))            -> solve (machine (true,((fun _->()),(fun _->()))))
	| InsertCoin(Stop(b1,b2, machine))               -> solve (machine ())
	    
    end

end
