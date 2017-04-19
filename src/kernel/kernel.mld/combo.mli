(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Interfaces_basic
open Basic
open Variables
open Theories
open Register
open Specs
       

(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.DataType
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)

module type Value = sig
  include PH
  val noValue : t
end 

type ('a,'b) convV = {
    injV : 'a -> 'b;
    projV: 'b -> 'a
  }

type _ typedList =
  | El  : unit typedList
  | Cons: 'a Termstructures.Register.t * 'b typedList -> ('a*'b) typedList

                                                                 
module type State = sig

  module DT      : DataType

  module Value   : Value

  val tsHandlers : DT.t typedList
                                       
  val modules : ('termdata -> DT.t)
                -> (Value.t,'value) convV
                -> (module GTheoryDSType with type Term.datatype = 'termdata
                                          and type Value.t  = 'value
                                          and type Assign.t = 'assign)
                -> ('termdata*'value*'assign) Theories.Register.Modules.t list
end

    
val theory_add : (_*(_*_*_*_)) Tags.t
                 -> (module State)
                 -> (module State)

open Prop
       
module Init(PlDS : Interfaces_plugin.PlugDSType) : sig

  module Make(MyTheory: Interfaces_theory.DecProc
              with type DS.formulae = PlDS.UF.t Formulae.FormulaF.generic)
         : sig
    module FE : (Interfaces_plugin.FrontEndType  with type IForm.datatype = PlDS.UF.t
			                         and  type FSet.ps     = PlDS.UFSet.t
			                         and  type ASet.ps     = PlDS.UASet.t)
    val machine : Formulae.FormulaB.t -> (FE.seqU FE.seq -> 'a FE.address) -> 'a FE.output
  end                                                                                    

  module State : State
                   
end
