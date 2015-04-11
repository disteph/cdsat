(**********************************************************)
(* This file contains the specification of theory modules *)
(**********************************************************)

exception TypingError of string
exception ModelError of string

type sort = [ `Prop | `Rat | `Term ]

type forParser = { names: string list }

type ('symbol,'a) forParsing =
    { arity     : 'symbol -> sort*(sort list);
      multiary  : ( 'symbol -> 'a list->'a list)->'symbol->'a list->'a;
      sortParse : string  -> sort;
      symbParse : string  -> 'symbol list}

(* Module type for a theory signature *)

module type SigType = sig
  type symbol
  val forParser : forParser
  val forParsing: (symbol,'a) forParsing
end


(* type of a model structure for a theory signature 
(to be used for parsing):
sigsymb_i : an interpretation for signature symbols,
decsymb_i  : an interpretation for declared symbols
*)

type ('symbol,'t) structureType = 
    { sigsymb_i   : 'symbol -> 't list -> 't;
      decsymb_i   : sort -> string  -> 't list -> 't;
      boundsymb_i : int -> sort -> 't;
      quantif_i   : bool -> sort -> 't -> 't                 
    }

(* Type of interpretation functions for applied symbols, pertaining to
the signature (sigsymb) or declared in the input problem (decsymb).
The arguments are:
- the string to be interpreted as a symbol
- its expected sort
- its given list of arguments
For declared symbols, there is an extra argument, which is the
declared signature for the symbol (for signature symbols, that
information is to be found in the signature). *)

type 't interpretType = 
    { sigsymb : string -> sort -> (sort->'t) list -> 't ;
      decsymb : string -> sort -> (sort->'t) list -> (string * (string list)) -> 't;
      boundsymb : int -> string -> sort -> 't;
      quantif : bool -> string list -> 't -> 't
    }

(* Type of a Theory with Decision Procedure *)

module type ThDecProc = sig

  (* Theory signature *)
  module Sig : SigType

  (* Implem of atoms and consistency checks, as required by kernel *)
  include Kernel.Interfaces_I.DecProc

  (* Suggested plugin to be used for proof-search *)
  val sugPlugin:(module Plugins.Type with type iliterals = IAtom.t
                                     and  type literals  = IAtom.Atom.t
                                     and  type delsubsts = IAtom.DSubst.t) option

  (* A model structure to be used for parsing, depending on an
  implementation of formulae, with a list of illustrative examples *)
  module Structure(F:Kernel.Formulae.FormulaType with type lit=IAtom.Atom.t) :
  sig
    type t
    val st      : (Sig.symbol,t) structureType
    val toform  : t->F.t
    val examples: ((unit->F.t)*bool) list
  end
end

