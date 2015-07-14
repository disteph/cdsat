open Format

open Kernel
open Prop

open Interfaces_theory
open Formulae
open Interfaces_plugin

module DS = Gplugins_tools.ListDS.Generate

module Strategy(FE:FrontEndType with type IForm.datatype = DS.UF.t
				and  type FSet.ps     = DS.UFSet.t
				and  type ASet.ps     = DS.UASet.t) = struct

  open DS
  open FE
  include Gplugins_tools.Utils.FEext(FE)
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
  let noaddress: data address = fun _ -> () 
  let initial_data _ = noaddress

  let rec solve = function
    | Jackpot ans                                  -> ans
    | InsertCoin(Notify(_,_,_,machine,_))          -> solve (machine (true,noaddress,accept,fNone))
    | InsertCoin(AskFocus(_,_,p,b,_,machine,_))    -> begin
      match FSet.forPlugin p with
      | [] when b -> solve (machine (Restore(noaddress,accept,fNone)))
      | []   -> solve (machine (ConsistencyCheck(noaddress,accept,fNone)))
      | a::l -> solve (machine (Focus(a,(noaddress,noaddress),accept,fNone)))
    end
    | InsertCoin(AskSide (_,_,machine,_))          -> solve (machine (true,(noaddress,noaddress)))
    | InsertCoin(Stop(b1,b2, machine))             -> solve (machine ())
      
end
