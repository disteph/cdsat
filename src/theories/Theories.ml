(**********************************************************)
(* This file contains the specification of theory modules *)
(**********************************************************)

exception TypingError of string
exception ModelError of string

type 'sort forParser =
    { names: string list ;
      prop: 'sort}

type ('sort,'symbol,'a) forParsing =
    { arity     : 'symbol -> 'sort*('sort list);
      multiary  : ('symbol->'a list->'a list)->'symbol->'a list->'a;
      sortParse : string  -> 'sort;
      symbParse : string  -> 'symbol list}

(* Module type for a theory signature *)

module type SigType = sig
  type sort
  type symbol
  val forParser : sort forParser
  val forParsing: (sort,symbol,'a) forParsing
end


(* type of a model structure for a theory signature 
(to be used for parsing):
symb_i : an interpretation for signature symbols,
var_i  : an interpretation for declared symbols
*)

type ('sort,'symbol,'t) structureType = 
    { sigsymb_i: 'symbol -> 't list -> 't;
      decsymb_i: 'sort -> string  -> 't list -> 't}

(* Type of interpretation functions for applied symbols, pertaining to
the signature (sigsymb) or declared in the input problem (decsymb).
The arguments are:
- the string to be interpreted as a symbol
- its expected sort
- its given list of arguments
For declared symbols, there is an extra argument, which is the
declared signature for the symbol (for signature symbols, that
information is to be found in the signature). *)

type ('sort,'t) interpretType = 
    { sigsymb : string -> 'sort -> ('sort->'t) list -> 't ;
      decsymb : string -> 'sort -> ('sort->'t) list -> (string * string list) -> 't}

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
    val examples: ((unit->F.t)*bool) list
  end
end

