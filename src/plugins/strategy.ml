open Kernel

open Formulae
open Collection
open Sequents

module type User = sig

  (* A user should provide an implementation of formulae, an
     implementation of sets of formulae, and an implementation of
     sets of atoms *)

  module UF: FormulaImplem
  module UFSet: CollectImplem with type e = UF.t
  module UASet: ACollectImplem

  (* A user should provide a strategy: given the datastructures of a
     FrontEnd (implementation of sequents, answers, outputs, etc), the
     strategy provides a function solve that should convert a
     temporary answer (output) into a final answer (t).  See the
     default implementation in module MyNaive *)

  module Strategy: 
    functor (FE:FrontEndType with module F=UF and module FSet=UFSet and module ASet=UASet) -> 
      (sig
	 (* A user can embark his own data on the proof-search.
	    Set it to unit if not used *)
	 type data
	 val initial_data:data
	 val solve : data FE.output -> FE.t
       end)
end
