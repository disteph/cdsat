(********************************************************)
(* This file contains the description of theory modules *)
(********************************************************)

type 'sort forParser =
    { names: string list ;
      prop: 'sort}

type ('sort,'symbol) forParsing =
    { arity     : 'symbol -> 'sort*('sort list)*('symbol option);
      sortParse : string  -> 'sort;
      symbParse : string  -> 'symbol list}

(* Module type for a theory signature *)

module type SigType = sig
  type sort
  type symbol
  val forParser : sort forParser
  val forParsing: (sort,symbol) forParsing
end


(* type of a model structure for a theory signature 
(to be used for parsing):
symb_i : an interpretation for symbols,
var_i  : an interpretation for (sorted) variables
*)

type ('sort,'symbol,'t) structureType = 
    { symb_i: 'symbol -> 't list -> 't;
      var_i : string  -> 'sort   -> 't}

(* Type of a Theory with Decision Procedure *)

module type ThDecProc = sig

  (* Theory signature *)
  module Sig : SigType

  (* Implem of atoms and consistency checks, as required by kernel *)
  include Kernel.Interfaces.DecProc

  (* Suggested plugin to be used for proof-search *)
  val sugPlugin:(module Plugins.Type with type literals = Atom.t)option

  (* A model structure to be used for parsing, depending on an
  implementation of formulae, with a list of illustrative examples *)
  module Structure(F:Kernel.Interfaces.PrintableFormulaType with type lit=Atom.t) :
  sig
    type t
    val st      : (Sig.sort,Sig.symbol,t) structureType
    val toform  : t->F.t
    val examples: (F.t*bool) list
  end
end
