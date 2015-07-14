open Kernel
open Prop
open Interfaces_theory
open Formulae
open Interfaces_plugin

exception PluginAbort of string

module type Type = sig

  (* A plugin should provide an implementation of formulae, an
     implementation of sets of formulae, and an implementation of
     sets of atoms *)

  module DS : PlugDSType

  (* A plugin should provide a strategy: given the datastructures of a
     FrontEnd (implementation of sequents, answers, outputs, etc), the
     strategy provides a function solve that should convert a
     temporary answer (output) into a final answer (t).  See the
     default implementation in module MyNaive *)

  module Strategy(FE:FrontEndType with type IForm.datatype = DS.UF.t
				  and  type FSet.ps     = DS.UFSet.t
				  and  type ASet.ps     = DS.UASet.t)
    : sig
      (* A user can embark his own data on the proof-search.
	 Set it to unit if not used *)
      type data
      val initial_data : FE.Seq.t -> bool list -> data
      val solve        : data FE.output -> FE.answer
    end
end
